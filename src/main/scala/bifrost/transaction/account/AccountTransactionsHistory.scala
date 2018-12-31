package bifrost.transaction.account

import bifrost.transaction.Transaction
import bifrost.transaction.box.proposition.Proposition

trait AccountTransactionsHistory[P <: Proposition, TX <: Transaction[P]] {
  def accountTransactions(id: P): Array[TX]
}
