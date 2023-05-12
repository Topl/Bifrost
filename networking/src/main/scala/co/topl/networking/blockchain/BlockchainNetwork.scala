package co.topl.networking.blockchain

import cats.effect._
import cats.effect.implicits._
import cats.effect.std.Random
import co.topl.networking.p2p._
import fs2._
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger

import scala.concurrent.duration._

object BlockchainNetwork {

  /**
   * Launches a P2P Network that runs blockchain operations
   * @param host The host to bind to
   * @param bindPort The port to bind to
   * @param localPeer The local peer description
   * @param remotePeers A stream of remote peers to connect to
   * @param clientHandler A handler for each peer client
   * @param serverF A server of data to each peer
   * @return A P2PNetwork
   */
  def make[F[_]: Async: Random](
    host:          String,
    bindPort:      Int,
    localPeer:     LocalPeer,
    remotePeers:   Stream[F, DisconnectedPeer],
    clientHandler: BlockchainPeerHandlerAlgebra[F],
    serverF:       ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]]
  ): Resource[F, P2PServer[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("Bifrost.P2P.Blockchain").toResource
      p2pServer <- FS2P2PServer.make[F](
        host,
        bindPort,
        localPeer,
        remotePeers,
        (peer, socket) =>
          SocketLeader
            .fromSocket(socket.readN, socket.write)
            .timeout(5.seconds)
            .toResource
            .flatMap(socketLeader =>
              BlockchainSocketHandler
                .make[F](serverF, clientHandler.usePeer)(peer, socketLeader, socket.reads, socket.writes)
            )
      )
    } yield p2pServer

}
