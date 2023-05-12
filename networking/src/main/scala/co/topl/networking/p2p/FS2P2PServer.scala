package co.topl.networking.p2p

import cats.data.OptionT
import cats.effect.Async
import cats.effect.Resource
import cats.effect.implicits._
import cats.implicits._
import com.comcast.ip4s._
import fs2.Stream
import fs2.concurrent.Topic
import fs2.io.net._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

object FS2P2PServer {

  def make[F[_]: Async](
    host:        String,
    port:        Int,
    localPeer:   LocalPeer,
    remotePeers: Stream[F, DisconnectedPeer],
    peerHandler: (ConnectedPeer, Socket[F]) => Resource[F, Unit]
  ): Resource[F, P2PServer[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("Bifrost.P2P").toResource
      topic                        <- Resource.make(Topic[F, PeerConnectionChange])(_.close.void)
      _                            <- eventLogger(topic)
      sockets                      <- socketsStream[F](host, port)
      _                            <- server(sockets)(topic, peerHandler)
      _                            <- client(remotePeers, localPeer)(topic, peerHandler)
      _                            <- Logger[F].info(s"Bound P2P at host=$host port=$port").toResource
    } yield new P2PServer[F] {

      def peerChanges: F[Topic[F, PeerConnectionChange]] =
        topic.pure[F]

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
    } yield Network[F].server(parsedHost.some, parsedPort.some)

  private def server[F[_]: Async](sockets: Stream[F, Socket[F]])(
    peerChangesTopic: Topic[F, PeerConnectionChange],
    peerHandler:      (ConnectedPeer, Socket[F]) => Resource[F, Unit]
  ) =
    sockets
      .evalMap(socket =>
        socket.remoteAddress
          .map(address => ConnectedPeer(RemoteAddress(address.host.toUriString, address.port.value), (0, 0)))
          .tupleRight(socket)
      )
      .evalTap { case (peer, _) =>
        peerChangesTopic.publish1(
          PeerConnectionChanges.InboundConnectionInitializing(peer.remoteAddress)
        )
      }
      .map { case (peer, socket) =>
        Stream.resource(
          for {
            _ <- peerChangesTopic.publish1(PeerConnectionChanges.ConnectionEstablished(peer)).toResource
            _ <- peerHandler(peer, socket)
          } yield ()
        )
      }
      .parJoinUnbounded
      .compile
      .drain
      .background

  private def client[F[_]: Async](
    remotePeers: Stream[F, DisconnectedPeer],
    localPeer:   LocalPeer
  )(peerChangesTopic: Topic[F, PeerConnectionChange], peerHandler: (ConnectedPeer, Socket[F]) => Resource[F, Unit]) =
    remotePeers
      .filterNot(_.remoteAddress == localPeer.localAddress)
      .evalTap(disconnectedPeer =>
        peerChangesTopic.publish1(PeerConnectionChanges.OutboundConnectionInitializing(disconnectedPeer.remoteAddress))
      )
      .map(disconnected =>
        Stream.resource(
          for {
            host <- OptionT
              .fromOption[F](Host.fromString(disconnected.remoteAddress.host))
              .getOrRaise(new IllegalArgumentException("Invalid destinationHost"))
              .toResource
            port <- OptionT
              .fromOption[F](Port.fromInt(disconnected.remoteAddress.port))
              .getOrRaise(new IllegalArgumentException("Invalid destinationPort"))
              .toResource
            socket <- Network[F].client(SocketAddress(host, port))
            connected = ConnectedPeer(disconnected.remoteAddress, disconnected.coordinate)
            _ <- peerChangesTopic.publish1(PeerConnectionChanges.ConnectionEstablished(connected)).toResource
            _ <- peerHandler(connected, socket)
          } yield ()
        )
      )
      .parJoinUnbounded
      .compile
      .drain
      .background

  private def eventLogger[F[_]: Async: Logger](peerChangesTopic: Topic[F, PeerConnectionChange]) =
    peerChangesTopic.subscribeUnbounded
      .evalTap {
        case PeerConnectionChanges.ConnectionEstablished(peer) =>
          Logger[F].info(s"Connection established with peer=$peer")
        case PeerConnectionChanges.ConnectionClosed(peer, reason) =>
          reason match {
            case Some(reason) => Logger[F].warn(reason)(s"Connection closed with peer=$peer")
            case _            => Logger[F].info(s"Connection closed with peer=$peer")
          }
        case PeerConnectionChanges.InboundConnectionInitializing(peer) =>
          Logger[F].info(s"Inbound connection initializing with peer=$peer")
        case PeerConnectionChanges.OutboundConnectionInitializing(peer) =>
          Logger[F].info(s"Outbound connection initializing with peer=$peer")
      }
      .compile
      .drain
      .background

}
