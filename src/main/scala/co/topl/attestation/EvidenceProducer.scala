package co.topl.attestation

import co.topl.attestation.Evidence.EvidenceTypePrefix
import co.topl.attestation.proposition.Proposition

/**
  * EvidenceProducer is a type-class that must be implemented by a proposition in order to generate the evidence
  * for that proposition. The evidence is then used to construct an address that holds outputs from a transaction.
 *
  * @tparam P a proposition that is used to encumber a UTXO
  */
sealed trait EvidenceProducer[P <: Proposition] {
  def generateEvidence (prop: P): Evidence
}

object EvidenceProducer {
  def apply[P](implicit ev: EvidenceProducer[P]): EvidenceProducer[P] = ev
  def instance[P](f: P => Evidence): EvidenceProducer[P] = new EvidenceProducer[P] {
    override def generateEvidence (prop: P): Evidence = f(prop)
  }

  object syntax {
    implicit final class ProducerOps[P: EvidenceProducer] ( private val value: P) {
      def generateEvidence: Evidence = EvidenceProducer[P].generateEvidence(value)
    }
  }
}