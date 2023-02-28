package co.topl.networking.p2p

import akka.actor.ActorSystem
import akka.stream.{KillSwitches, SharedKillSwitch}
import akka.stream.scaladsl._
import akka.util.ByteString
import cats.Parallel
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

  def make[F[_]: Async: Parallel: Logger: FToFuture, Client](
    host:        String,
    port:        Int,
    localPeer:   LocalPeer,
    remotePeers: Stream[F, DisconnectedPeer],
    peerHandler: ConnectedPeer => F[Flow[ByteString, ByteString, F[Client]]]
  )(implicit system: ActorSystem): Resource[F, P2PServer[F, Client]] = {
    import system.dispatcher
    for {
      topic      <- Resource.make(Topic[F, PeerConnectionChange[Client]])(_.close.void)
      killSwitch <- Resource.make(Sync[F].delay(KillSwitches.shared("p2p-server")))(k => Sync[F].delay(k.shutdown()))
      addConnectionChange =
        (change: PeerConnectionChange[Client]) =>
          EitherT(topic.publish1(change)).leftMap(_ => new IllegalStateException("Topic closed")).rethrowT
      addConnectedPeer =
        (connectedPeer: ConnectedPeer, clientF: F[Client]) =>
          clientF.flatMap(c => addConnectionChange(PeerConnectionChanges.ConnectionEstablished(connectedPeer, c)))
      peerHandlerFlowWithRemovalF = makePeerHandlerFlowWithRemovalF(peerHandler, addConnectionChange, killSwitch)
      _ <- outboundConnectionsProcess(
        localPeer,
        remotePeers,
        addConnectionChange,
        peerHandlerFlowWithRemovalF,
        addConnectedPeer,
        killSwitch
      )
      _ <- inboundConnectionsProcess(
        host,
        port,
        addConnectionChange,
        peerHandlerFlowWithRemovalF,
        addConnectedPeer
      )
      _ <- Resource.eval(Logger[F].info("P2P Server Running"))
    } yield new P2PServer[F, Client] {

      def peerChanges: F[Topic[F, PeerConnectionChange[Client]]] =
        topic.pure[F]

      val localAddress: F[RemoteAddress] =
        localPeer.localAddress.pure[F]
    }
  }

  private def makePeerHandlerFlowWithRemovalF[F[_]: FToFuture, Client](
    peerHandler:         ConnectedPeer => F[Flow[ByteString, ByteString, F[Client]]],
    addConnectionChange: PeerConnectionChange[Client] => F[Unit],
    killSwitch:          SharedKillSwitch
  ) =
    (connectedPeer: ConnectedPeer) =>
      Flow
        .futureFlow(implicitly[FToFuture[F]].apply(peerHandler(connectedPeer)))
        .viaMat(killSwitch.flow)(Keep.left)
        .alsoTo(
          Sink.onComplete[ByteString](result =>
            implicitly[FToFuture[F]].apply(
              addConnectionChange(PeerConnectionChanges.ConnectionClosed(connectedPeer, result.failed.toOption))
            )
          )
        )

  private def inboundConnectionsProcess[F[_]: Async: Logger: FToFuture, Client](
    host:                        String,
    port:                        Int,
    addConnectionChange:         PeerConnectionChange[Client] => F[Unit],
    peerHandlerFlowWithRemovalF: ConnectedPeer => Flow[ByteString, ByteString, Future[F[Client]]],
    addConnectedPeer:            (ConnectedPeer, F[Client]) => F[Unit]
  )(implicit system: ActorSystem, ec: ExecutionContext): Resource[F, Unit] =
    Resource
      .make(
        Tcp()
          .bind(host, port)
          .tapAsyncF(1)(conn =>
            Logger[F].info(s"Inbound peer connection from address=${conn.remoteAddress}") *>
            addConnectionChange(
              PeerConnectionChanges.InboundConnectionInitializing(
                RemoteAddress(conn.remoteAddress.getHostString, conn.remoteAddress.getPort)
              )
            )
          )
          .mapAsync(1) { conn =>
            val connectedPeer =
              ConnectedPeer(RemoteAddress(conn.remoteAddress.getHostString, conn.remoteAddress.getPort), (0, 0))
            conn.handleWith(peerHandlerFlowWithRemovalF(connectedPeer)).tupleLeft(connectedPeer)
          }
          .tapAsyncF(1)((addConnectedPeer.apply _).tupled)
          .toMat(Sink.ignore)(Keep.both)
          .withLogAttributes
          .liftTo[F]
          .flatMap(f => Async[F].fromFuture(Async[F].delay(f._1)).tupleRight(f._2))
      ) { case (binding, completionFuture) =>
        Logger[F].info(s"Unbinding P2P server") *>
        Async[F].fromFuture(Async[F].delay(binding.unbind())) >> Async[F]
          .fromFuture(Async[F].delay(completionFuture))
          .void
      }
      .void

  private def outboundConnectionsProcess[F[_]: Async: Parallel: Logger, Client](
    localPeer:                   LocalPeer,
    remotePeers:                 Stream[F, DisconnectedPeer],
    offerConnectionChange:       PeerConnectionChange[Client] => F[Unit],
    peerHandlerFlowWithRemovalF: ConnectedPeer => Flow[ByteString, ByteString, Future[F[Client]]],
    addPeer:                     (ConnectedPeer, F[Client]) => F[Unit],
    killSwitch:                  SharedKillSwitch
  )(implicit system: ActorSystem): Resource[F, F[Outcome[F, Throwable, Unit]]] =
    Async[F].background(
      remotePeers
        .filterNot(_.remoteAddress == localPeer.localAddress)
        .evalTap(disconnectedPeer =>
          Logger[F].info(s"Initializing outbound peer connection to address=${disconnectedPeer.remoteAddress}") *>
          offerConnectionChange(PeerConnectionChanges.OutboundConnectionInitializing(disconnectedPeer.remoteAddress))
        )
        .map(disconnected => ConnectedPeer(disconnected.remoteAddress, disconnected.coordinate))
        .evalMap(connectedPeer =>
          Async[F]
            .blocking(
              InetSocketAddress.createUnresolved(connectedPeer.remoteAddress.host, connectedPeer.remoteAddress.port)
            )
            .flatMap(socketAddress =>
              Async[F].start(
                Async[F]
                  .fromFuture(
                    Tcp()
                      .outgoingConnection(socketAddress)
                      .viaMat(killSwitch.flow)(Keep.left)
                      .joinMat(
                        Flow[ByteString].viaMat(peerHandlerFlowWithRemovalF(connectedPeer))(Keep.right)
                      )(Keep.right)
                      .liftTo[F]
                  )
                  .flatMap(addPeer(connectedPeer, _))
              )
            )
        )
        .compile
        .toList
        .flatMap(_.parTraverse(_.joinWithUnit).void)
    )
}
