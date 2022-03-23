package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.{Attributes, QueueCompletionResult, QueueOfferResult}
import akka.stream.scaladsl._
import akka.util.ByteString
import cats.effect._
import cats.effect.kernel.Sync
import cats.implicits._
import cats.~>

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.util.{Failure, Try}

object AkkaP2PServer {

  def make[F[_]: Async: *[_] ~> Future, Client](
    host:            String,
    port:            Int,
    localAddress:    InetSocketAddress,
    remotePeers:     Source[InetSocketAddress, _],
    peerHandler:     (ConnectedPeer, ConnectionLeader) => F[Flow[ByteString, ByteString, Client]]
  )(implicit system: ActorSystem): F[P2PServer[F, Client]] = {
    def localAddress_ = localAddress
    import system.dispatcher
    for {
      connectedPeersRef <- Ref.of[F, Map[ConnectedPeer, Client]](Map.empty)
      (newConnectionsQueue, newConnectionsSource) = Source
        .queue[(ConnectedPeer, Client)](128)
        .toMat(BroadcastHub.sink)(Keep.both)
        .run()
      addPeersSink = Flow[(ConnectedPeer, Client)]
        .alsoTo(Sink.foreach(newConnectionsQueue.offer(_) match {
          case QueueOfferResult.Failure(e)                             => throw e
          case QueueOfferResult.Dropped | QueueOfferResult.QueueClosed => throw new IllegalStateException
          case QueueOfferResult.Enqueued                               =>
        }))
        .to(
          Sink.foreachAsync[(ConnectedPeer, Client)](1)(connection =>
            implicitly[F ~> Future].apply(connectedPeersRef.update(_ + connection))
          )
        )
      remotePeersSinkF = (connectedPeer: ConnectedPeer) =>
        Sink.onComplete[ByteString](_ => implicitly[F ~> Future].apply(connectedPeersRef.update(_ - connectedPeer)))
      incomingConnections = Tcp().bind(host, port)
      peerHandlerFlowWithRemovalF = (connectedPeer: ConnectedPeer) =>
        ConnectionLeaderFlow(leader =>
          Flow.futureFlow(
            implicitly[F ~> Future].apply(
              peerHandler(connectedPeer, leader).map(_.alsoTo(remotePeersSinkF(connectedPeer)))
            )
          )
        )
          .mapMaterializedValue(_.flatten)
          .pure[F]
      serverBindingRunnableGraph = incomingConnections
        .log("Inbound peer connection", _.remoteAddress)
        .mapAsync(1) { conn =>
          val connectedPeer = ConnectedPeer(conn.remoteAddress)
          implicitly[F ~> Future]
            .apply(
              peerHandlerFlowWithRemovalF(connectedPeer)
                .map(
                  _.alsoTo(
                    Sink.onComplete(printCompletion(connectedPeer, _))
                  )
                )
            )
            .flatMap(conn.handleWith(_))
            .map(connectedPeer -> _)
        }
        .alsoTo(addPeersSink)
        .alsoTo(
          Sink.onComplete {
            case Failure(exception) => println(s"Error + $exception")
            case _                  => println("Done")
          }
        )
        .toMat(Sink.ignore)(Keep.both)
        .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
      (serverBindingFuture, serverBindingCompletionFuture) = serverBindingRunnableGraph.run()
      serverBinding <- Async[F].fromFuture(serverBindingFuture.pure[F])
      outboundConnectionsRunnableGraph: RunnableGraph[Future[Unit]] =
        remotePeers
          .filterNot(_ == localAddress)
          .map(ConnectedPeer)
          .log("Initializing connection", identity)
          .mapAsync(1)(connectedPeer =>
            implicitly[F ~> Future]
              .apply(peerHandlerFlowWithRemovalF(connectedPeer))
              .map(
                _.alsoTo(Sink.onComplete(printCompletion(connectedPeer, _)))
              )
              .flatMap(flow =>
                Tcp()
                  .outgoingConnection(connectedPeer.remoteAddress)
                  .joinMat(flow)((_, r) => r.tupleLeft(connectedPeer))
                  .run()
              )
          )
          .alsoTo(addPeersSink)
          .alsoTo(
            Sink.onComplete {
              case Failure(exception) =>
                println(s"Error + $exception")
                exception.printStackTrace()
              case _ => println("Done outgoing all")
            }
          )
          .toMat(Sink.seq)(Keep.right)
          .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel, onFinish = Logging.InfoLevel))
          .mapMaterializedValue(_.void)
      remoteConnectionsCompletionFuture = outboundConnectionsRunnableGraph.run()
      _ = println("P2P Server Running")
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

  private def printCompletion[T](connectedPeer: ConnectedPeer, result: Try[T]): Unit =
    result match {
      case Failure(exception) =>
        println(s"connectedPeer=$connectedPeer error=$exception")
        exception.printStackTrace()
      case _ =>
        println(s"connectedPeer=$connectedPeer Done outgoing single")
    }
}
