package co.topl.address.serialization

import co.topl.address.{PublicKeyAddress, ToplAddress}
import co.topl.address.ToplAddress.AddressContent
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ToplAddressSerializer extends BifrostSerializer[ToplAddress] {

  override def serialize(obj: ToplAddress, w: Writer): Unit = {
    /* addressBytes: Array[Byte] */
    w.putBytes(obj.addressBytes)
  }

  override def parse(r: Reader): ToplAddress = {
    val addressType: Byte = r.getByte()
    val addressContent: Array[Byte] = r.getBytes(ToplAddress.addressContentLength)

    addressType match {
      case PublicKeyAddress.addressTypePrefix => new PublicKeyAddress(AddressContent @@ addressContent)
      case _ => throw new Exception("Invalid address: Unsupported address type " + addressType)
    }
  }
}