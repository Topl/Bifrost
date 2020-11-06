package co.topl.nodeView.state

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.Transaction

import scala.util.Try

trait StateFeature

trait TransactionValidation[TX <: Transaction[_, _ <: Proposition, _ <: Proof[_]]] extends StateFeature {
  def filterValid(txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def isValid(tx: TX): Boolean = validate(tx).isSuccess

  def validate(tx: TX): Try[Unit]
}
