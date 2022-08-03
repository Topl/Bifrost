package co.topl.algebras

import co.topl.models.{BlockHeaderV2, Transaction, TypedIdentifier}

/**
 * An interaction layer intended for users/clients of a blockchain node.
 */
trait ToplRpc[F[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]

  def currentMempool(): F[Set[TypedIdentifier]]

  def fetchBlockHeader(blockId: TypedIdentifier): F[Option[BlockHeaderV2]]
}
