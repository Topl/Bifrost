package co.topl.attestation.evidence

import co.topl.attestation.evidence.Evidence.{ EvidenceContent, EvidenceTypePrefix }
import co.topl.attestation.proposition.{ Proposition, PublicKey25519Proposition }
import co.topl.attestation.secrets.{ PrivateKey25519, Secret }

case class PublicKeyAddress[P <: Proposition] ( private[attestation] val typePrefix: EvidenceTypePrefix,
                                           private[attestation] val content: EvidenceContent)
                                         (implicit val addressEncoder: EvidenceEncoder) extends Evidence {

}

object PublicKeyAddress {
  val addressTypePrefixCurve25519: EvidenceTypePrefix = 1: Byte

  def apply (prop: PublicKey25519Proposition)(implicit addressEncoder: EvidenceEncoder): PublicKeyAddress[PrivateKey25519] =
    new PublicKeyAddress(addressTypePrefixCurve25519, EvidenceContent @@ prop.pubKeyBytes)
}
