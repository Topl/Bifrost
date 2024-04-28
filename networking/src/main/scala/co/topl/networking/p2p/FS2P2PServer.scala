package co.topl.networking.p2p

import cats.data.OptionT
import cats.effect.{Async, Resource}
import cats.effect.implicits._
import cats.effect.std.Random
import cats.implicits._
import co.topl.crypto.signing.Ed25519
import co.topl.models.Bytes
import co.topl.models.p2p._
import co.topl.networking.SocketAddressOps
import co.topl.networking.blockchain.NetworkProtocolVersions
import com.comcast.ip4s._
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.net._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object FS2P2PServer {

  def make[F[_]: Async: Random](
    host:                    String,
    port:                    Int,
    localPeer:               LocalPeer,
    remotePeers:             Stream[F, DisconnectedPeer],
    peerHandler:             (ConnectedPeer, Socket[F]) => Resource[F, Unit],
    peersStatusChangesTopic: Topic[F, PeerConnectionChange],
    ed25519Resource:         Resource[F, Ed25519]
  ): Resource[F, P2PServer[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("Bifrost.P2P").toResource
      _                            <- eventLogger(peersStatusChangesTopic)
      sockets                      <- socketsStream[F](host, port)
      peerInfoExtractor <- PeerIdentity
        .extractor(Ed25519.SecretKey(localPeer.p2pSK.toByteArray), ed25519Resource)
        .map(f =>
          (socket: Socket[F]) =>
            f(socket).rethrow.flatMap {
              case (id, true) => PeerVersion.extractor[F](NetworkProtocolVersions.Local)(socket).rethrow.tupleLeft(id)
              case (id, _)    => (id, NetworkProtocolVersions.V0).pure[F]
            }
        )
        .toResource
      _ <- server(sockets)(peersStatusChangesTopic, peerHandler, peerInfoExtractor)
      _ <- client(remotePeers, localPeer, peerInfoExtractor)(peersStatusChangesTopic, peerHandler)
      _ <- Logger[F].info(s"Bound P2P at host=$host port=$port").toResource
    } yield new P2PServer[F] {

      def peerChanges: F[Topic[F, PeerConnectionChange]] =
        peersStatusChangesTopic.pure[F]

      def localAddress: F[RemoteAddress] =
        localPeer.localAddress.pure[F]
    }

  private def socketsStream[F[_]: Async](host: String, port: Int) =
    for {
      parsedHost <-
        OptionT
          .fromOption[F](Host.fromString(host))
          .getOrRaise(new IllegalArgumentException("Invalid bindHost"))
          .toResource
      parsedPort <-
        OptionT
          .fromOption[F](Port.fromInt(port))
          .getOrRaise(new IllegalArgumentException("Invalid bindPort"))
          .toResource
    } yield Network.forAsync[F].server(parsedHost.some, parsedPort.some)

  private def server[F[_]: Async](sockets: Stream[F, Socket[F]])(
    peerChangesTopic:  Topic[F, PeerConnectionChange],
    peerHandler:       (ConnectedPeer, Socket[F]) => Resource[F, Unit],
    peerInfoExtractor: Socket[F] => F[(Bytes, Bytes)]
  ) =
    sockets
      .evalMap(socket => socket.remoteAddress.map(_.asRemoteAddress).tupleRight(socket))
      .evalTap { case (remoteAddress, socket) =>
        for {
          localAddress <- socket.localAddress.map(_.asRemoteAddress)
          _ <- peerChangesTopic.publish1(
            PeerConnectionChanges.InboundConnectionInitializing(remoteAddress, localAddress)
          )
        } yield ()
      }
      .map { case (remoteAddress, socket) =>
        Stream
          .resource(
            for {
              localAddress <- socket.localAddress.map(_.asRemoteAddress).toResource
              (peerId, peerVersion) <- peerInfoExtractor(socket).onError { case e =>
                peerChangesTopic
                  .publish1(PeerConnectionChanges.ConnectionClosed(DisconnectedPeer(remoteAddress, none), e.some))
                  .void
              }.toResource
              peer = ConnectedPeer(remoteAddress, peerId, peerVersion)
              disconnectedPeer = DisconnectedPeer(remoteAddress, peerId.some)
              _ <- peerChangesTopic.publish1(PeerConnectionChanges.ConnectionEstablished(peer, localAddress)).toResource
              _ <- peerHandler(peer, socket)
                .onError { case e =>
                  peerChangesTopic
                    .publish1(PeerConnectionChanges.ConnectionClosed(disconnectedPeer, e.some))
                    .void
                    .toResource
                }
              _ <- (socket.endOfOutput *> socket.endOfInput).toResource
              _ <- peerChangesTopic
                .publish1(PeerConnectionChanges.ConnectionClosed(disconnectedPeer, none))
                .toResource
            } yield ()
          )
          .voidError
      }
      .parJoinUnbounded
      .compile
      .drain
      .background

  private def client[F[_]: Async: Logger](
    remotePeers:       Stream[F, DisconnectedPeer],
    localPeer:         LocalPeer,
    peerInfoExtractor: Socket[F] => F[(Bytes, Bytes)]
  )(peerChangesTopic: Topic[F, PeerConnectionChange], peerHandler: (ConnectedPeer, Socket[F]) => Resource[F, Unit]) =
    remotePeers
      .filterNot(_.remoteAddress == localPeer.localAddress)
      .evalTap(disconnectedPeer =>
        peerChangesTopic.publish1(PeerConnectionChanges.OutboundConnectionInitializing(disconnectedPeer.remoteAddress))
      )
      .map(disconnected =>
        Stream
          .resource(
            for {
              host <- OptionT
                .fromOption[F](Host.fromString(disconnected.remoteAddress.host))
                .getOrRaise(new IllegalArgumentException("Invalid destinationHost"))
                .toResource
              port <- OptionT
                .fromOption[F](Port.fromInt(disconnected.remoteAddress.port))
                .getOrRaise(new IllegalArgumentException("Invalid destinationPort"))
                .toResource
              _ <- Logger[F].info(show"Initiate connection to ${disconnected.toString}").toResource
              socket <- Network
                .forAsync[F]
                .client(SocketAddress(host, port))
                .onError { case e =>
                  peerChangesTopic
                    .publish1(PeerConnectionChanges.ConnectionClosed(disconnected, e.some))
                    .void
                    .toResource
                }
              // TODO: Compare against disconnectedPeer.p2pVK?
              (peerId, peerVersion) <- peerInfoExtractor(socket).onError { case e =>
                peerChangesTopic.publish1(PeerConnectionChanges.ConnectionClosed(disconnected, e.some)).void
              }.toResource
              newDisconnected = disconnected.copy(p2pVK = peerId.some)
              _ <- Logger[F].info(show"Established connection to ${newDisconnected.toString}").toResource
              _ <-
                if (disconnected.p2pVK != newDisconnected.p2pVK)
                  peerChangesTopic
                    .publish1(PeerConnectionChanges.ChangedRemotePeer(disconnected, newDisconnected))
                    .void
                    .toResource
                else Resource.unit[F]
              connected = ConnectedPeer(disconnected.remoteAddress, peerId, peerVersion)
              localAddress <- socket.localAddress.map(_.asRemoteAddress).toResource
              _ <- peerChangesTopic
                .publish1(PeerConnectionChanges.ConnectionEstablished(connected, localAddress))
                .toResource
              _ <- peerHandler(connected, socket)
                .onError { case e =>
                  peerChangesTopic
                    .publish1(PeerConnectionChanges.ConnectionClosed(newDisconnected, e.some))
                    .void
                    .toResource
                }
              _ <- (socket.endOfOutput *> socket.endOfInput).toResource
              _ <- peerChangesTopic
                .publish1(PeerConnectionChanges.ConnectionClosed(newDisconnected, none))
                .toResource
            } yield ()
          )
          .voidError
      )
      .parJoinUnbounded
      .compile
      .drain
      .background

  private def eventLogger[F[_]: Async: Logger](peerChangesTopic: Topic[F, PeerConnectionChange]) =
    peerChangesTopic.subscribeUnbounded
      .evalTap {
        case PeerConnectionChanges.ConnectionEstablished(peer, localAddress) =>
          Logger[F].info(s"Connection established local[$localAddress] <=> remote[$peer]")
        case PeerConnectionChanges.ConnectionClosed(peer, reason) =>
          reason match {
            case Some(error) => Logger[F].warn(error)(s"Connection closed with peer=$peer")
            case _           => Logger[F].info(s"Connection closed with peer=$peer")
          }
        case PeerConnectionChanges.InboundConnectionInitializing(remotePeer, localAddress) =>
          Logger[F].info(s"Inbound connection initializing into $localAddress, from remote peer $remotePeer")
        case PeerConnectionChanges.OutboundConnectionInitializing(peer) =>
          Logger[F].info(s"Outbound connection initializing with peer=$peer")
        case PeerConnectionChanges.RemotePeerApplicationLevel(peer, applicationLevelEnabled) =>
          Logger[F].info(s"Remote peer $peer application level is $applicationLevelEnabled")
        case PeerConnectionChanges.ChangedRemotePeer(oldPeer, newPeer) =>
          Logger[F].info(s"Remote peer $oldPeer had been changed to $newPeer")
      }
      .compile
      .drain
      .background

}
