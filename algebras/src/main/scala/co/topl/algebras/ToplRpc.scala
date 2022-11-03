package co.topl.algebras

import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}

/**
 * Topl Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */
trait ToplRpc[F[_], S[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]

  def currentMempool(): F[Set[TypedIdentifier]]

  def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeaderV2]]

  def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBodyV2]]

  def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]]

  def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]]

  def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]]

  def synchronizationTraversal(): F[S[SynchronizationTraversalStep]]
}
