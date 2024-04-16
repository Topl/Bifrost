package co.topl.networking.blockchain

import cats.Applicative
import cats.effect._
import cats.effect.implicits._
import cats.effect.std.{Mutex, Random}
import cats.implicits._
import co.topl.crypto.signing.Ed25519
import co.topl.networking.legacy.{ConnectionLeader, LegacyBlockchainSocketHandler}
import co.topl.networking.multiplexer.MultiplexedReaderWriter
import co.topl.networking.p2p._
import fs2._
import fs2.concurrent.Topic
import org.typelevel.log4cats.Logger
import org.typelevel.log4cats.slf4j.Slf4jLogger
import co.topl.typeclasses.implicits._

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
   * @param peersStatusChangesTopic topic for notifying about changes in remote peers
   * @return A P2PNetwork
   */
  def make[F[_]: Async: Random](
    host:                    String,
    bindPort:                Int,
    localPeer:               LocalPeer,
    remotePeers:             Stream[F, DisconnectedPeer],
    clientHandler:           BlockchainPeerHandlerAlgebra[F],
    serverF:                 ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]],
    peersStatusChangesTopic: Topic[F, PeerConnectionChange],
    ed25519Resource:         Resource[F, Ed25519]
  ): Resource[F, P2PServer[F]] =
    for {
      implicit0(logger: Logger[F]) <- Slf4jLogger.fromName("Bifrost.P2P.Blockchain").toResource
      p2pServer <- FS2P2PServer.make[F](
        host,
        bindPort,
        localPeer,
        remotePeers,
        (peer, socket) =>
          for {
            portQueues <- BlockchainMultiplexedBuffers.make[F]
            readerWriter = MultiplexedReaderWriter(socket)
            peerCache <- PeerStreamBuffer.make[F]
            server    <- serverF(peer)
            _ <-
              if (peer.networkVersion == NetworkProtocolVersions.V0) {
                // Run the legacy P2P implementation
                (
                  Logger[F].info(show"Using legacy network protocol for peer=${peer.p2pVK}").toResource >>
                  ConnectionLeader
                    .fromSocket(socket.readN, socket.write)
                    .timeout(5.seconds)
                    .toResource
                    .flatMap(connectionLeader =>
                      LegacyBlockchainSocketHandler
                        .make[F](serverF, clientHandler.usePeer)(
                          peer,
                          connectionLeader,
                          socket.reads,
                          socket.writes,
                          socket.isOpen.ifM(socket.endOfOutput >> socket.endOfInput, Applicative[F].unit)
                        )
                    )
                )
              } else {
                // Run the "newer" P2P implementation
                Stream
                  .eval(Mutex[F])
                  .flatMap(mutex =>
                    new BlockchainSocketHandler[F](
                      server,
                      portQueues,
                      readerWriter,
                      peerCache,
                      mutex,
                      peer,
                      3.seconds
                    ).client
                  )
                  .evalMap(clientHandler.usePeer(_).use_)
                  .compile
                  .drain
                  .toResource
              }
            _ <- Logger[F].info(show"Done with peer=${peer.p2pVK}").toResource
          } yield (),
        peersStatusChangesTopic,
        ed25519Resource
      )
    } yield p2pServer

}
