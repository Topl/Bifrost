package co.topl.algebras

import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}

/**
 * An interaction layer intended for users/clients of a blockchain node.
 */
trait ToplRpc[F[_], S[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]

  def currentMempool(): F[Set[TypedIdentifier]]

  def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeaderV2]]

  def fetchBlockBody(blockId: TypedIdentifier): F[Option[BlockBodyV2]]

  def fetchTransaction(transactionId: TypedIdentifier): F[Option[Transaction]]

  def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]]

  def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]]

  def synchronizationTraversal(currentHead: TypedIdentifier): F[S[SynchronizationTraversalStep]]
}
