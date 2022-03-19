package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.util.ByteString
import cats.effect._
import cats.implicits._
import cats.~>

import java.net.InetSocketAddress
import scala.concurrent.Future
import scala.util.Failure

object AkkaP2PServer {

  def make[F[_]: Async: *[_] ~> Future](
    host:            String,
    port:            Int,
    localAddress:    InetSocketAddress,
    remotePeers:     Source[InetSocketAddress, _],
    peerHandler:     ConnectedPeer => F[Flow[ByteString, ByteString, NotUsed]]
  )(implicit system: ActorSystem): F[P2PServer[F]] = {
    def localAddress_ = localAddress
    import system.dispatcher
    for {
      connectedPeersRef <- Ref.of[F, Set[ConnectedPeer]](Set.empty)
      addPeersSink = Sink.foreachAsync[ConnectedPeer](1)(connection =>
        implicitly[F ~> Future].apply(connectedPeersRef.update(_ + connection))
      )
      remotePeersSinkF = (connectedPeer: ConnectedPeer) =>
        Sink.onComplete[ByteString](_ =>
          //
          implicitly[F ~> Future].apply(connectedPeersRef.update(_ - connectedPeer))
        )
      incomingConnections = Tcp().bind(host, port)
      peerHandlerFlowWithRemovalF = (connectedPeer: ConnectedPeer) =>
        peerHandler(connectedPeer).map(_.alsoTo(remotePeersSinkF(connectedPeer)))
      serverBindingRunnableGraph = incomingConnections
        .wireTap(p =>
          //
          println(s"Inbound peer connection ${p.remoteAddress}")
        )
        .alsoTo(Flow[Tcp.IncomingConnection].map(c => ConnectedPeer(c.remoteAddress)).to(addPeersSink))
        .mapAsync(1)(conn =>
          implicitly[F ~> Future]
            .apply(peerHandlerFlowWithRemovalF(ConnectedPeer(conn.remoteAddress)).map(conn.handleWith))
        )
        .alsoTo(
          Sink.onComplete(d =>
            d match {
              case Failure(exception) => println(s"Error + $exception")
              case _                  => println("Done")
            }
          )
        )
        .toMat(Sink.ignore)(Keep.both)
      (serverBindingFuture, serverBindingCompletionFuture) = serverBindingRunnableGraph.run()
      serverBinding <- Async[F].fromFuture(serverBindingFuture.pure[F])
      outboundConnectionsRunnableGraph: RunnableGraph[Future[Unit]] =
        remotePeers
          .map(ConnectedPeer)
          .alsoTo(addPeersSink)
          .wireTap(connectedPeer => println(s"connectedPeer=$connectedPeer Initializing Connection"))
          .map(connectedPeer =>
            implicitly[F ~> Future]
              .apply(peerHandlerFlowWithRemovalF(connectedPeer))
              .map(
                _.alsoTo(
                  Sink.onComplete {
                    case Failure(exception) =>
                      println(s"connectedPeer=$connectedPeer error=$exception")
                      exception.printStackTrace()
                    case _ =>
                      println(s"connectedPeer=$connectedPeer Done outgoing single")
                  }
                )
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
}
