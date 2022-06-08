package co.topl.algebras

import co.topl.models.{BlockHeaderV2, Transaction, TypedIdentifier}

trait ToplRpc[F[_], Stream[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]
  def blockAdoptions(): F[Stream[TypedIdentifier]]
  def fetchHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
  def currentMempool(): F[Set[TypedIdentifier]]
}
