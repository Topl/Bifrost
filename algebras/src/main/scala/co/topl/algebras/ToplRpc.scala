package co.topl.algebras

import akka.NotUsed
import co.topl.models.{BlockHeaderV2, Transaction, TypedIdentifier}

trait ToplRpc[F[_], Stream[_]] {
  def broadcastTx(transaction: Transaction): F[Unit]
  def blockAdoptions(): F[Stream[TypedIdentifier]]

  def fetchHeader(id: TypedIdentifier): F[Option[BlockHeaderV2]]
}
