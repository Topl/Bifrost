package co.topl.nodeView.state

import co.topl.modifier.transaction.Transaction

import scala.util.Try

trait StateFeature

trait TransactionValidation extends StateFeature {
  def filterValid[TX: Transaction] (txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def isValid[TX: Transaction] (tx: TX): Boolean = validate(tx).isSuccess

  def validate[TX: Transaction] (transaction: TX): Try[Unit]
}
