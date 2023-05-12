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
      blockchainSocketHandler = BlockchainSocketHandler.make[F](serverF, clientHandler.usePeer) _
      p2pServer <- FS2P2PServer.make[F](
        host,
        bindPort,
        localPeer,
        remotePeers,
        (peer, socket) =>
          SocketLeader
            .fromSocket(socket)
            .timeout(5.seconds)
            .toResource
            .flatMap(blockchainSocketHandler(peer, _, socket))
      )
    } yield p2pServer

}
