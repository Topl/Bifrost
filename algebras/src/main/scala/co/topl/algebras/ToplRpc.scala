package co.topl.algebras

import co.topl.models.Transaction

trait ToplRpc[F[_]] {
  def broadcastTransaction(transaction: Transaction): F[Unit]
}
