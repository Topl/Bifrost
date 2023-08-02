package co.topl.networking.p2p

import cats.data.OptionT
import cats.effect.Async
import cats.effect.Resource
import cats.effect.implicits._
import cats.effect.std.Queue
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
    peerHandler: (ConnectedPeer, Socket[F]) => Resource[F, Unit],
    closedPeers: Queue[F, ConnectedPeer]
  ): Resource[F, P2PServer[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("Bifrost.P2P").toResource
      topic                        <- Resource.make(Topic[F, PeerConnectionChange])(_.close.void)
      _                            <- eventProcessor(topic, closedPeers)
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
    } yield Network.forAsync[F].server(parsedHost.some, parsedPort.some)

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
            _      <- peerChangesTopic.publish1(PeerConnectionChanges.ConnectionEstablished(peer)).toResource
            result <- peerHandler(peer, socket).attempt
            _ <- peerChangesTopic
              .publish1(PeerConnectionChanges.ConnectionClosed(peer, result.swap.toOption))
              .toResource
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
            socketEither <- Network.forAsync[F].client(SocketAddress(host, port)).attempt
            connected = ConnectedPeer(disconnected.remoteAddress, disconnected.coordinate)
            result <- socketEither match {
              case Left(error) => Resource.pure[F, Either[Throwable, Unit]](Either.left[Throwable, Unit](error))
              case Right(socket) =>
                peerChangesTopic.publish1(PeerConnectionChanges.ConnectionEstablished(connected)).toResource >>
                peerHandler(connected, socket).attempt
            }
            _ <- peerChangesTopic
              .publish1(PeerConnectionChanges.ConnectionClosed(connected, result.swap.toOption))
              .toResource
          } yield ()
        )
      )
      .parJoinUnbounded
      .compile
      .drain
      .background

  private def eventProcessor[F[_]: Async: Logger](
    peerChangesTopic: Topic[F, PeerConnectionChange],
    closedPeers:      Queue[F, ConnectedPeer]
  ) =
    peerChangesTopic.subscribeUnbounded
      .evalTap {
        case PeerConnectionChanges.ConnectionEstablished(peer) =>
          Logger[F].info(s"Connection established with peer=$peer")
        case PeerConnectionChanges.ConnectionClosed(peer, reason) =>
          {
            reason match {
              case Some(error) => Logger[F].warn(error)(s"Connection closed with peer=$peer")
              case _           => Logger[F].info(s"Connection closed with peer=$peer")
            }
          } >> closedPeers.offer(peer)
        case PeerConnectionChanges.InboundConnectionInitializing(peer) =>
          Logger[F].info(s"Inbound connection initializing with peer=$peer")
        case PeerConnectionChanges.OutboundConnectionInitializing(peer) =>
          Logger[F].info(s"Outbound connection initializing with peer=$peer")
      }
      .compile
      .drain
      .background

}
