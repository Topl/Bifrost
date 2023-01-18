package co.topl.algebras

import co.topl.models.TypedIdentifier

/**
 * Topl Rpc
 * An interaction layer intended for users/clients of a blockchain node.
 * @tparam F Effect type
 * @tparam S Canonical head changes Synchronization Traversal Container, Ex: Stream, Seq
 */
trait ToplRpc[F[_], S[_]] {
  def broadcastTransaction(transaction: co.topl.proto.models.Transaction): F[Unit]

  def currentMempool(): F[Set[TypedIdentifier]]

  def fetchBlockHeader(blockId: TypedIdentifier): F[Option[co.topl.consensus.models.BlockHeader]]

  def fetchBlockBody(blockId: TypedIdentifier): F[Option[co.topl.node.models.BlockBody]]

  def fetchTransaction(transactionId: TypedIdentifier): F[Option[co.topl.proto.models.Transaction]]

  def blockIdAtHeight(height: Long): F[Option[TypedIdentifier]]

  def blockIdAtDepth(depth: Long): F[Option[TypedIdentifier]]

  def synchronizationTraversal(): F[S[SynchronizationTraversalStep]]
}
