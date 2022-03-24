package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.scaladsl._
import akka.stream.{Attributes, BoundedSourceQueue, QueueOfferResult}
import akka.util.ByteString
import cats.effect._
import cats.effect.kernel.Sync
import cats.implicits._
import cats.{~>, Applicative, MonadThrow}
import org.typelevel.log4cats.Logger

import java.net.InetSocketAddress
import scala.concurrent.Future

object AkkaP2PServer {

  def make[F[_]: Async: Logger: *[_] ~> Future, Client](
    host:            String,
    port:            Int,
    localAddress:    InetSocketAddress,
    remotePeers:     Source[InetSocketAddress, _],
    peerHandler:     ConnectedPeer => F[Flow[ByteString, ByteString, F[Client]]]
  )(implicit system: ActorSystem): F[P2PServer[F, Client]] = {
    def localAddress_ = localAddress
    import system.dispatcher
    def offerToQueue[T](queue: BoundedSourceQueue[T], value: T): F[Unit] =
      queue.offer(value) match {
        case QueueOfferResult.Failure(e) => MonadThrow[F].raiseError(e)
        case QueueOfferResult.Dropped | QueueOfferResult.QueueClosed =>
          MonadThrow[F].raiseError(new IllegalStateException)
        case QueueOfferResult.Enqueued => Applicative[F].unit
      }
    for {
      connectedPeersRef <- Ref.of[F, Map[ConnectedPeer, Client]](Map.empty)
      (newConnectionsQueue, newConnectionsSource) = Source
        .queue[(ConnectedPeer, Client)](128)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
      addPeersSink = Flow[(ConnectedPeer, F[Client])]
        .to(
          Sink.foreachAsync(1)(connection =>
            implicitly[F ~> Future].apply(
              connection._2
                .tupleLeft(connection._1)
                .flatMap { case (peer, client) =>
                  (
                    offerToQueue(newConnectionsQueue, (peer, client)),
                    connectedPeersRef.update(_.updated(peer, client))
                  ).tupled.void
                }
            )
          )
        )
      peerHandlerFlowWithRemovalF = (connectedPeer: ConnectedPeer) =>
        Flow
          .futureFlow(implicitly[F ~> Future].apply(peerHandler(connectedPeer)))
          .alsoTo(
            Sink.onComplete[ByteString](_ => implicitly[F ~> Future].apply(connectedPeersRef.update(_ - connectedPeer)))
          )
      serverBindingRunnableGraph = Tcp()
        .bind(host, port)
        .log("Inbound peer connection", _.remoteAddress)
        .mapAsync(1) { conn =>
          val connectedPeer = ConnectedPeer(conn.remoteAddress)
          conn.handleWith(peerHandlerFlowWithRemovalF(connectedPeer)).tupleLeft(connectedPeer)
        }
        .alsoTo(addPeersSink)
        .toMat(Sink.ignore)(Keep.both)
        .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
      outboundConnectionsRunnableGraph: RunnableGraph[Future[Unit]] =
        remotePeers
          .filterNot(_ == localAddress)
          .map(ConnectedPeer)
          .log("Initializing connection", identity)
          .mapAsync(1)(connectedPeer =>
            Tcp()
              .outgoingConnection(connectedPeer.remoteAddress)
              .joinMat(peerHandlerFlowWithRemovalF(connectedPeer))((_, r) => r.tupleLeft(connectedPeer))
              .run()
          )
          .alsoTo(addPeersSink)
          .toMat(Sink.seq)(Keep.right)
          .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
          .mapMaterializedValue(_.void)
      (serverBindingFuture, serverBindingCompletionFuture) = serverBindingRunnableGraph.run()
      serverBinding <- Async[F].fromFuture(serverBindingFuture.pure[F])
      remoteConnectionsCompletionFuture = outboundConnectionsRunnableGraph.run()
      _ <- Logger[F].info("P2P Server Running")
    } yield new P2PServer[F, Client] {
      def stop(): F[Unit] =
        Async[F]
          .fromFuture(
            Sync[F].delay(
              (serverBinding.unbind() >> serverBinding.whenUnbound >> serverBindingCompletionFuture)
                .zip(remoteConnectionsCompletionFuture)
            )
          )
          .void

      def connectedPeers(): F[Map[ConnectedPeer, Client]] =
        connectedPeersRef.get

      override def newConnectedPeers: F[Source[(ConnectedPeer, Client), NotUsed]] =
        newConnectionsSource.pure[F]

      val localAddress: F[InetSocketAddress] =
        localAddress_.pure[F]
    }
  }
}
