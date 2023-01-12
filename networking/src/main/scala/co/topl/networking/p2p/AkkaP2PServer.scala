package co.topl.networking.p2p

import akka.actor.ActorSystem
import akka.stream.scaladsl._
import akka.util.ByteString
import cats._
import cats.data.EitherT
import cats.effect._
import cats.implicits._
import co.topl.catsakka._
import org.typelevel.log4cats.Logger
import fs2._
import fs2.concurrent.Topic

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
    remotePeers:     Stream[F, DisconnectedPeer],
    peerHandler:     ConnectedPeer => F[Flow[ByteString, ByteString, F[Client]]]
  )(implicit system: ActorSystem): Resource[F, P2PServer[F, Client]] = {
    def localAddress_ = localPeer.localAddress
    import system.dispatcher
    for {
      topic <- Resource.make(Topic[F, PeerConnectionChange[Client]])(_.close.void)
      offerConnectionChange = (change: PeerConnectionChange[Client]) =>
        EitherT(topic.publish1(change)).leftMap(_ => new IllegalStateException("Topic closed")).rethrowT
      addPeersSink = (connectedPeer: ConnectedPeer, clientF: F[Client]) =>
        clientF.flatMap(c => offerConnectionChange(PeerConnectionChanges.ConnectionEstablished(connectedPeer, c)))
      peerHandlerFlowWithRemovalF = makePeerHandlerFlowWithRemovalF(peerHandler, offerConnectionChange)
      serverBindingRunnableGraph = makeServerBindingRunnableGraph(
        host,
        port,
        offerConnectionChange,
        peerHandlerFlowWithRemovalF,
        addPeersSink
      )
      _ <- makeOutboundConnectionsRunnableGraph(
        localPeer,
        remotePeers,
        offerConnectionChange,
        peerHandlerFlowWithRemovalF,
        addPeersSink
      )
      (serverBindingFuture, serverBindingCompletionFuture) <- Resource.eval(serverBindingRunnableGraph.liftTo[F])
      _ <- Resource.make(Async[F].fromFuture(serverBindingFuture.pure[F]))(f => Async[F].fromFuture(f.unbind().pure[F]))
      _ <- Resource.eval(Logger[F].info("P2P Server Running"))
    } yield new P2PServer[F, Client] {

      def peerChanges: F[Topic[F, PeerConnectionChange[Client]]] =
        topic.pure[F]

      val localAddress: F[InetSocketAddress] =
        localAddress_.pure[F]
    }
  }

  private def makePeerHandlerFlowWithRemovalF[F[_]: FToFuture, Client](
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
    addPeer:                     (ConnectedPeer, F[Client]) => F[Unit]
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
      .tapAsyncF(1)((addPeer.apply _).tupled)
      .toMat(Sink.ignore)(Keep.both)
      .withLogAttributes

  private def makeOutboundConnectionsRunnableGraph[F[_]: Async: FToFuture: Logger, Client](
    localPeer:                   LocalPeer,
    remotePeers:                 Stream[F, DisconnectedPeer],
    offerConnectionChange:       PeerConnectionChange[Client] => F[Unit],
    peerHandlerFlowWithRemovalF: ConnectedPeer => Flow[ByteString, ByteString, Future[F[Client]]],
    addPeer:                     (ConnectedPeer, F[Client]) => F[Unit]
  )(implicit system:             ActorSystem, ec: ExecutionContext) =
    Async[F].background(
      remotePeers
        .filterNot(_.remoteAddress == localPeer.localAddress)
        .evalTap(disconnectedPeer =>
          Logger[F].info(s"Initializing outbound peer connection to address=${disconnectedPeer.remoteAddress}") *>
          offerConnectionChange(PeerConnectionChanges.OutboundConnectionInitializing(disconnectedPeer.remoteAddress))
        )
        .map(disconnected => ConnectedPeer(disconnected.remoteAddress, disconnected.coordinate))
        .evalMap(connectedPeer =>
          Tcp()
            .outgoingConnection(connectedPeer.remoteAddress)
            .joinMat(
              Flow[ByteString]
                .viaMat(peerHandlerFlowWithRemovalF(connectedPeer))(Keep.right)
            )((_, r) => r.tupleLeft(connectedPeer))
            .liftTo[F]
            .flatMap(future => Async[F].fromFuture(future.pure[F]))
        )
        .evalTap((addPeer.apply _).tupled)
        .compile
        .drain
    )
}
