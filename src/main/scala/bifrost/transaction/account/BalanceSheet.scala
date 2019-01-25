package bifrost.transaction.account

import bifrost.transaction.box.proposition.Proposition


trait BalanceSheet[P <: Proposition] {

  def balance(id: P, height: Option[Int] = None): Long

}
