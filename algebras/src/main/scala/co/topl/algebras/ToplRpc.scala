package co.topl.algebras

import co.topl.models.Transaction

trait ToplRpc[F[_]] {
  def broadcastTx(transaction: Transaction): F[Unit]
}
