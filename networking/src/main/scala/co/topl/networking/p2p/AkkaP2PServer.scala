package co.topl.networking.p2p

import akka.NotUsed
import akka.actor.ActorSystem

import akka.stream.scaladsl._
import akka.util.ByteString
import cats._
import cats.effect._
import cats.effect.kernel.Sync
import cats.implicits._
import co.topl.catsakka._
import org.typelevel.log4cats.Logger

import java.net.InetSocketAddress

import scala.concurrent.{ExecutionContext, Future}

/**
 * Interprets P2PServer[F, Client] using akka-stream. Binds to the requested host/port to accept incoming connections,
 * and initializes outbound connections using the provided remotePeers Source.  Each new connection (both inbound and
 * outbound) is handled by the given `peerHandler`, which will emit a Client on materialization
 */
object AkkaP2PServer {

  def make[F[_]: Async: Logger: FToFuture, Client](
    host:            String,
    port:            Int,
    localPeer:       LocalPeer,
    remotePeers:     Source[DisconnectedPeer, _],
    peerHandler:     ConnectedPeer => F[Flow[ByteString, ByteString, F[Client]]]
  )(implicit system: ActorSystem): F[P2PServer[F, Client]] = {
    def localAddress_ = localPeer.localAddress
    import system.dispatcher
    for {
      ((offerConnectionChange, completeConnectionChanges), connectionChangesSource) <- Source
        .backpressuredQueue[F, PeerConnectionChange[Client]](128)
        .toMat(BroadcastHub.sink)(Keep.both)
        .liftTo[F]
      addPeersSink = makeAddPeersSink(offerConnectionChange)
      peerHandlerFlowWithRemovalF = makePeerHandlerFlowWithRemovalF(peerHandler, offerConnectionChange)
      serverBindingRunnableGraph = makeServerBindingRunnableGraph(
        host,
        port,
        offerConnectionChange,
        peerHandlerFlowWithRemovalF,
        addPeersSink
      )
      outboundConnectionsRunnableGraph =
        makeOutboundConnectionsRunnableGraph(
          localPeer,
          remotePeers,
          offerConnectionChange,
          peerHandlerFlowWithRemovalF,
          addPeersSink
        )
      (serverBindingFuture, serverBindingCompletionFuture) <- serverBindingRunnableGraph.liftTo[F]
      serverBinding                                        <- Async[F].fromFuture(serverBindingFuture.pure[F])
      remoteConnectionsCompletionFuture                    <- outboundConnectionsRunnableGraph.liftTo[F]
      _                                                    <- Logger[F].info("P2P Server Running")
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

      def peerChanges: F[Source[PeerConnectionChange[Client], NotUsed]] =
        connectionChangesSource.pure[F]

      val localAddress: F[InetSocketAddress] =
        localAddress_.pure[F]
    }
  }

  private def makeAddPeersSink[F[_]: Monad: FToFuture, Client](
    offerConnectionChange: PeerConnectionChange[Client] => F[Unit]
  ) =
    Flow[(ConnectedPeer, F[Client])]
      .tapAsyncF(1) { case (connectedPeer, clientF) =>
        clientF
          .map(PeerConnectionChanges.ConnectionEstablished(connectedPeer, _))
          .flatMap(offerConnectionChange)
      }
      .to(Sink.ignore)

  private def makePeerHandlerFlowWithRemovalF[F[_]: Monad: FToFuture, Client](
    peerHandler:           ConnectedPeer => F[Flow[ByteString, ByteString, F[Client]]],
    offerConnectionChange: PeerConnectionChange[Client] => F[Unit]
  ) =
    (connectedPeer: ConnectedPeer) =>
      Flow
        .futureFlow(implicitly[FToFuture[F]].apply(peerHandler(connectedPeer)))
        .alsoTo(
          Sink.onComplete[ByteString](result =>
            implicitly[FToFuture[F]].apply(
              offerConnectionChange(PeerConnectionChanges.ConnectionClosed(connectedPeer, result.failed.toOption))
            )
          )
        )

  private def makeServerBindingRunnableGraph[F[_]: Monad: Logger: FToFuture, Client](
    host:                        String,
    port:                        Int,
    offerConnectionChange:       PeerConnectionChange[Client] => F[Unit],
    peerHandlerFlowWithRemovalF: ConnectedPeer => Flow[ByteString, ByteString, Future[F[Client]]],
    addPeersSink:                Sink[(ConnectedPeer, F[Client]), NotUsed]
  )(implicit system:             ActorSystem, ec: ExecutionContext) =
    Tcp()
      .bind(host, port)
      .tapAsyncF(1)(conn =>
        Logger[F].info(s"Inbound peer connection from address=${conn.remoteAddress}") *>
        offerConnectionChange(PeerConnectionChanges.InboundConnectionInitializing(conn.remoteAddress))
      )
      .mapAsync(1) { conn =>
        val connectedPeer = ConnectedPeer(conn.remoteAddress, (0, 0))
        conn.handleWith(peerHandlerFlowWithRemovalF(connectedPeer)).tupleLeft(connectedPeer)
      }
      .alsoTo(addPeersSink)
      .toMat(Sink.ignore)(Keep.both)
      .withLogAttributes

  private def makeOutboundConnectionsRunnableGraph[F[_]: Async: FToFuture: Logger, Client](
    localPeer:                   LocalPeer,
    remotePeers:                 Source[DisconnectedPeer, _],
    offerConnectionChange:       PeerConnectionChange[Client] => F[Unit],
    peerHandlerFlowWithRemovalF: ConnectedPeer => Flow[ByteString, ByteString, Future[F[Client]]],
    addPeersSink:                Sink[(ConnectedPeer, F[Client]), NotUsed]
  )(implicit system:             ActorSystem, ec: ExecutionContext) =
    remotePeers
      .filterNot(_.remoteAddress == localPeer.localAddress)
      .tapAsyncF(1)(disconnectedPeer =>
        Logger[F].info(s"Initializing outbound peer connection to address=${disconnectedPeer.remoteAddress}") *>
        offerConnectionChange(PeerConnectionChanges.OutboundConnectionInitializing(disconnectedPeer.remoteAddress))
      )
      .map(disconnected => ConnectedPeer(disconnected.remoteAddress, disconnected.coordinate))
      .mapAsyncF(1)(connectedPeer =>
        Tcp()
          .outgoingConnection(connectedPeer.remoteAddress)
          .joinMat(
            Flow[ByteString]
              .viaMat(peerHandlerFlowWithRemovalF(connectedPeer))(Keep.right)
          )((_, r) => r.tupleLeft(connectedPeer))
          .liftTo[F]
          .flatMap(future => Async[F].fromFuture(future.pure[F]))
      )
      .alsoTo(addPeersSink)
      .toMat(Sink.ignore)(Keep.right)
      .withLogAttributes
      .mapMaterializedValue(_.void)
}
