package co.topl.modifier.transaction

import co.topl.nodeView.state.StateReader

import scala.util.Try

trait TransactionValidation[TX] {
  //def filterValid (txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def semanticValidate (tx: TX, stateReader: StateReader): Try[Unit]
  def isSemanticallyValid (tx: TX, stateReader: StateReader): Boolean = semanticValidate(tx, stateReader).isSuccess

  def syntacticValidate (tx: TX): Try[Unit]
  def isSyntacticallyValid (tx: TX): Boolean = syntacticValidate(tx).isSuccess
}

object TransactionValidation {
  def apply[TX](implicit ev: TransactionValidation[TX]):  TransactionValidation[TX] = ev
}
