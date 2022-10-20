package co.topl.transactiongenerator.algebras

import co.topl.models.Transaction

trait TransactionBroadcaster[F[_]] {

  def broadcastTransaction(transaction: Transaction): F[Unit]

}
