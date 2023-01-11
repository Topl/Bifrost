package co.topl.algebras

import co.topl.models.{BlockBody, Transaction, TypedIdentifier}
import co.topl.consensus.models.{BlockHeader => ConsensusBlockHeader} // TODO remove rename, after remove models

/**
 * Topl Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */
trait ToplRpc[F[_], S[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]

  def currentMempool(): F[Set[TypedIdentifier]]

  def fetchBlockHeader(blockId: TypedIdentifier): F[Option[ConsensusBlockHeader]]

  def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBody]]

  def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]]

  def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]]

  def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]]

  def synchronizationTraversal(): F[S[SynchronizationTraversalStep]]
}
