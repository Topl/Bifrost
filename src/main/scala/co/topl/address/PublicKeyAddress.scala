package co.topl.address

import co.topl.address.ToplAddress.{AddressContent, AddressTypePrefix}

class PublicKeyAddress(override val content: AddressContent) extends ToplAddress(PublicKeyAddress.addressTypePrefix, content) {

  override def equals(obj: Any): Boolean = obj match {
    case pk: PublicKeyAddress => content sameElements pk.content
    case _ => false
  }
}

object PublicKeyAddress {
  val addressTypePrefix: AddressTypePrefix = 1: Byte
}
