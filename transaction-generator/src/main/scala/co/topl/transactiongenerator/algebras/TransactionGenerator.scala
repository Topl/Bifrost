package co.topl.transactiongenerator.algebras

import co.topl.models.Transaction

trait TransactionGenerator[F[_], G[_]] {
  def generateTransactions: F[G[Transaction]]
}
