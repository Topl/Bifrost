package co.topl.algebras

import co.topl.models.{BlockBodyV2, BlockHeaderV2, Transaction, TypedIdentifier}

trait ToplRpc[F[_], Stream[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]
  def blockAdoptions(): F[Stream[TypedIdentifier]]
  def fetchHeader(id:      TypedIdentifier): F[Option[BlockHeaderV2]]
  def fetchBody(id:        TypedIdentifier): F[Option[BlockBodyV2]]
  def fetchTransaction(id: TypedIdentifier): F[Option[Transaction]]
  def currentMempool(): F[Set[TypedIdentifier]]
}
