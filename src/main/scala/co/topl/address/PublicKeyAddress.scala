package co.topl.address

import co.topl.address.ToplAddress.{AddressContent, AddressTypePrefix}
import co.topl.crypto.proposition.PublicKey25519Proposition
import co.topl.crypto.{PrivateKey25519, Secret}

class PublicKeyAddress[S <: Secret] (typePrefix: AddressTypePrefix, content: AddressContent)
  extends ToplAddress[S](typePrefix, content) {

  override def equals(obj: Any): Boolean = obj match {
    case pk: PublicKeyAddress[_] => content sameElements pk.content
    case _ => false
  }
}

object PublicKeyAddress {
  val addressTypePrefixCurve25519: AddressTypePrefix = 1: Byte

  def apply (prop: PublicKey25519Proposition): PublicKeyAddress[PrivateKey25519] =
    new PublicKeyAddress(addressTypePrefixCurve25519, AddressContent @@ prop.pubKeyBytes)
}
