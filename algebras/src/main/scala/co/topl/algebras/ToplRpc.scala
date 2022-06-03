package co.topl.algebras

import akka.NotUsed
import co.topl.models.{Transaction, TypedIdentifier}

trait ToplRpc[F[_], Stream[_]] {
  def broadcastTx(transaction: Transaction): F[Unit]
  def blockAdoptions(): F[Stream[TypedIdentifier]]
}
