package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem
import akka.event.Logging
import akka.stream.Attributes
import akka.stream.scaladsl._
import akka.util.ByteString
import cats.effect._
import cats.implicits._
import cats.~>

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.util.{Failure, Try}

object AkkaP2PServer {

  def make[F[_]: Async: *[_] ~> Future](
    host:            String,
    port:            Int,
    localAddress:    InetSocketAddress,
    remotePeers:     Source[InetSocketAddress, _],
    peerHandler:     (ConnectedPeer, ConnectionLeader) => F[Flow[ByteString, ByteString, NotUsed]]
  )(implicit system: ActorSystem): F[P2PServer[F]] = {
    def localAddress_ = localAddress
    import system.dispatcher
    for {
      connectedPeersRef <- Ref.of[F, Set[ConnectedPeer]](Set.empty)
      addPeersSink = Sink.foreachAsync[ConnectedPeer](1)(connection =>
        implicitly[F ~> Future].apply(connectedPeersRef.update(_ + connection))
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
          .pure[F]
      serverBindingRunnableGraph = incomingConnections
        .log("Inbound peer connection", _.remoteAddress)
        .alsoTo(Flow[Tcp.IncomingConnection].map(c => ConnectedPeer(c.remoteAddress)).to(addPeersSink))
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
                .map(conn.handleWith)
            )
        }
        .alsoTo(
          Sink.onComplete(d =>
            d match {
              case Failure(exception) => println(s"Error + $exception")
              case _                  => println("Done")
            }
          )
        )
        .toMat(Sink.ignore)(Keep.both)
        .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel))
      (serverBindingFuture, serverBindingCompletionFuture) = serverBindingRunnableGraph.run()
      serverBinding <- Async[F].fromFuture(serverBindingFuture.pure[F])
      outboundConnectionsRunnableGraph: RunnableGraph[Future[Unit]] =
        remotePeers
          .filterNot(_ == localAddress)
          .map(ConnectedPeer)
          .alsoTo(addPeersSink)
          .log("Initializing connection", identity)
          .map(connectedPeer =>
            implicitly[F ~> Future]
              .apply(peerHandlerFlowWithRemovalF(connectedPeer))
              .map(
                _.alsoTo(Sink.onComplete(printCompletion(connectedPeer, _)))
              )
              .flatMap(flow => Tcp().outgoingConnection(connectedPeer.remoteAddress).join(flow).run())
          )
          .alsoTo(
            Sink.onComplete {
              case Failure(exception) =>
                println(s"Error + $exception")
                exception.printStackTrace()
              case _ => println("Done outgoing all")
            }
          )
          .toMat(Sink.seq)(Keep.right)
          .withAttributes(Attributes.logLevels(onElement = Logging.InfoLevel))
          .mapMaterializedValue(_.flatMap(_.sequence.void))
      remoteConnectionsCompletionFuture = outboundConnectionsRunnableGraph.run()
      _ = println("P2P Server Running")
    } yield new P2PServer[F] {
      def stop(): F[Unit] =
        Async[F]
          .fromFuture(
            Sync[F].delay(
              (serverBinding.unbind() >> serverBinding.whenUnbound >> serverBindingCompletionFuture)
                .zip(remoteConnectionsCompletionFuture)
            )
          )
          .void

      def connectedPeers(): F[Set[ConnectedPeer]] =
        connectedPeersRef.get

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
