package co.topl.address.serialization

import co.topl.address.ToplAddress.AddressContent
import co.topl.address.{PublicKeyAddress, ToplAddress}
import co.topl.crypto.Secret
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ToplAddressSerializer extends BifrostSerializer[ToplAddress[_ <: Secret]] {

  def serialize(obj: ToplAddress[_ <: Secret], w: Writer): Unit = {
    /* addressBytes: Array[Byte] */
    w.putBytes(obj.addressBytes)
  }

  def parse(r: Reader): ToplAddress[_ <: Secret] = {
    val addressType: Byte = r.getByte()
    val addressContent: Array[Byte] = r.getBytes(ToplAddress.addressContentLength)

    ToplAddress.matchType(addressType, AddressContent @@ addressContent)
  }
}