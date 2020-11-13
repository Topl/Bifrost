package co.topl.nodeView.state

import scala.util.Try

trait StateFeature

trait TransactionValidation[TX] extends StateFeature {
  def filterValid (txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def isValid (tx: TX): Boolean = validate(tx).isSuccess

  def validate (transaction: TX): Try[Unit]
}
