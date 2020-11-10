package co.topl.nodeView.state

import co.topl.attestation.EvidenceProducer
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.transaction.Transaction

import scala.util.Try

trait StateFeature

trait TransactionValidation extends StateFeature {
  def filterValid[P <: Proposition: EvidenceProducer] (txs: Seq[ Transaction[_, P, _, _]]): Seq[ Transaction[_, P, _, _]] = txs.filter(isValid)

  def isValid[P <: Proposition: EvidenceProducer] (tx:  Transaction[_, P, _, _]): Boolean = validate(tx).isSuccess

  def validate[P <: Proposition: EvidenceProducer] (tx: Transaction[_, P, _, _]): Try[Unit]
}
