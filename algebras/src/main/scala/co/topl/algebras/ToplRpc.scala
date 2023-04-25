package co.topl.algebras

import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockHeader
import co.topl.consensus.models.BlockId
import co.topl.node.models.BlockBody

/**
 * Topl Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */
trait ToplRpc[F[_], S[_]] {
  def broadcastTransaction(transaction: IoTransaction): F[Unit]

  def currentMempool(): F[Set[TransactionId]]

  def fetchBlockHeader(blockId: BlockId): F[Option[BlockHeader]]

  def fetchBlockBody(blockId: BlockId): F[Option[BlockBody]]

  def fetchTransaction(transactionId: TransactionId): F[Option[IoTransaction]]

  def blockIdAtHeight(height: Long): F[Option[BlockId]]

  def blockIdAtDepth(depth: Long): F[Option[BlockId]]

  def synchronizationTraversal(): F[S[SynchronizationTraversalStep]]
}
