package co.topl.nodeView.state

import co.topl.attestation.EvidenceProducer
import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.GenericBox

import scala.util.Try

trait StateFeature

trait TransactionValidation[TX <: Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: GenericBox[_]]] extends StateFeature {
  def filterValid[P <: Proposition: EvidenceProducer, PR <: Proof[P]](txs: Seq[TX]): Seq[TX] = txs.filter(isValid)

  def isValid[P <: Proposition: EvidenceProducer, PR <: Proof[P]](tx: TX): Boolean = validate(tx).isSuccess

  def validate[P <: Proposition: EvidenceProducer, PR <: Proof[P]](tx: TX): Try[Unit]
}
