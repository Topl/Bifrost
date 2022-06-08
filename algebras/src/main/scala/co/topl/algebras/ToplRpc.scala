package co.topl.algebras

import co.topl.models.{Transaction, TypedIdentifier}

trait ToplRpc[F[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]

  def currentMempool(): F[Set[TypedIdentifier]]
}
