package co.topl.attestation.evidence

import co.topl.attestation.evidence.Evidence.{ AddressContent, AddressTypePrefix }
import co.topl.attestation.proposition.PublicKey25519Proposition
import co.topl.attestation.secrets.{ PrivateKey25519, Secret }

case class PublicKeyAddress[S <: Secret] ( private[attestation] val addressTypePrefix: AddressTypePrefix,
                                           private[attestation] val content: AddressContent)
                                         (implicit val addressEncoder: AddressEncoder) extends Evidence[S]

object PublicKeyAddress {
  val addressTypePrefixCurve25519: AddressTypePrefix = 1: Byte

  def apply (prop: PublicKey25519Proposition)(implicit addressEncoder: AddressEncoder): PublicKeyAddress[PrivateKey25519] =
    new PublicKeyAddress(addressTypePrefixCurve25519, AddressContent @@ prop.pubKeyBytes)
}
