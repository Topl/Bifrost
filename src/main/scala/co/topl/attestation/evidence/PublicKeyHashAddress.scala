package co.topl.attestation.evidence

import co.topl.attestation.proposition.PublicKey25519Proposition
import co.topl.attestation.secrets.{ PrivateKey25519, Secret }
import scorex.crypto.hash.Blake2b256

case class PublicKeyHashAddress[S <: Secret] ( private[attestation] val typePrefix: AddressTypePrefix,
                                               private[attestation] val content : AddressContent)
                                             (implicit val addressEncoder: EvidenceEncoder) extends Evidence[S]

object PublicKeyHashAddress {
  val addressTypePrefixCurve25519: AddressTypePrefix = 2: Byte

  def apply (prop: PublicKey25519Proposition)(implicit addressEncoder: EvidenceEncoder): PublicKeyHashAddress[PrivateKey25519] = {
    val publicKeyHash = Blake2b256(prop.pubKeyBytes)
    new PublicKeyHashAddress(addressTypePrefixCurve25519, AddressContent @@ publicKeyHash)
  }
}
