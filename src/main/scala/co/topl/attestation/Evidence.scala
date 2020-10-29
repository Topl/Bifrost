package co.topl.attestation

import co.topl.attestation.Evidence.{EvidenceContent, EvidenceTypePrefix}
import supertagged.TaggedType

/**
  * Evidence type-class that must be implemented by a proposition in order to generate the evidence content
  * for tht proposition. The evidence content is then used to construct an address that holds outputs from a transaction.
  * @tparam P a proposition that is used to encumber a UTXO
  */
trait Evidence[P <: Proposition] {
  def generateEvidence (prop: P): EvidenceContent
  def typePrefix (prop: P): EvidenceTypePrefix = prop.typePrefix
}

object Evidence {
  def apply[P](implicit ev: Evidence[P]): Evidence[P] = ev
  def instance[P](f: P => EvidenceContent): Evidence[P] = (value: P) => f(value)

  object syntax {
    implicit final class Ops[P: Evidence](private val value: P) {
      def generateEvidence(implicit ev: Evidence[P]): EvidenceContent = ev.generateEvidence(value)
      def typePrefix(implicit ev: Evidence[P]): EvidenceTypePrefix = ev.typePrefix(value)
    }
  }

  // below are types and values used enforce the behavior of evidence
  type EvidenceTypePrefix = Byte

  object EvidenceContent extends TaggedType[Array[Byte]]
  type EvidenceContent = EvidenceContent.Type

  val contentLength = 32 //bytes (this is generally the output of a Blake2b-256 bit hash)
}