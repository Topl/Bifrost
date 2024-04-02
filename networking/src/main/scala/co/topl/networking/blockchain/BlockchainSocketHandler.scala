package co.topl.networking.blockchain

import cats.effect.{Async, Resource}
import cats.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.codecs.bytes.tetra.instances._
import co.topl.consensus.models.{BlockHeader, BlockId, SlotData}
import co.topl.networking._
import co.topl.networking.p2p.{ConnectedPeer, ConnectionLeader}
import co.topl.node.models._
import co.topl.typeclasses.implicits._
import fs2._
import org.typelevel.log4cats.Logger
import co.topl.networking.fsnetwork.P2PShowInstances._

object BlockchainSocketHandler {

  /**
   * Consumes the given Socket by applying Blockchain-specific Multiplexed Typed Protocols to serve the given application
   * functions.
   * @param peerServerF a function which creates a BlockchainPeerServer for the given ConnectedPeer
   * @param useClientAndPeer a function which consumes the BlockchainPeerClient to serve the application
   * @param peer The remote peer
   * @param reads the input stream
   * @param writes the output stream
   * @return a Resource which completes when all processing has completed
   */
  def make[F[_]: Async: Logger](
    peerServerF:      ConnectedPeer => Resource[F, BlockchainPeerServerAlgebra[F]],
    useClientAndPeer: BlockchainPeerClient[F] => Resource[F, Unit]
  )(
    peer:   ConnectedPeer,
    reads:  Stream[F, Byte],
    writes: Pipe[F, Byte, Nothing],
    close:  F[Unit]
  ): Resource[F, Unit] =
    peerServerF(peer)
      .map(server => createFactory(server, close))
      .flatMap(_.multiplexed(useClientAndPeer)(peer, leader, reads, writes))

}
