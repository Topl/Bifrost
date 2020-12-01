package co.topl.nodeView.state

import co.topl.modifier.transaction.Transaction

import scala.util.Try

trait StateFeature

trait TransactionValidation[TX <: Transaction] extends StateFeature {
  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def validate(tx: TX): Try[Unit]
}
