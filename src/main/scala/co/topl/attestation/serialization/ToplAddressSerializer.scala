package co.topl.attestation.serialization

import co.topl.attestation.AddressEncoder.NetworkPrefix
import co.topl.attestation.ToplAddress.{ AddressContent, AddressTypePrefix }
import co.topl.attestation.PublicKeyHashAddress
import co.topl.attestation.evidence.{ AddressEncoder, PublicKeyAddress, PublicKeyHashAddress, Evidence }
import co.topl.attestation.secrets.PrivateKey25519
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ToplAddressSerializer extends BifrostSerializer[Evidence] {

  def serialize( obj: Evidence, w: Writer): Unit = {
    /* networkType: Byte */
    w.put(obj.addressEncoder.networkPrefix)

    /* addressType: Byte */
    w.put(obj.addressTypePrefix)

    /* addressBytes: Array[Byte] */
    w.putBytes(obj.content)
  }

  def parse(r: Reader): Evidence = {
    val networkType: NetworkPrefix = r.getByte()
    val addressType: AddressTypePrefix = r.getByte()
    val addressContent: AddressContent = AddressContent @@ r.getBytes(Evidence.addressContentLength)

    implicit val addressEncoder: AddressEncoder = new AddressEncoder(networkType)
    addressType match {
      case prefix @ PublicKeyAddress.addressTypePrefixCurve25519     => new PublicKeyAddress[PrivateKey25519](prefix, AddressContent @@ addressContent)
      case prefix @ PublicKeyHashAddress.addressTypePrefixCurve25519 => new PublicKeyHashAddress[PrivateKey25519](prefix, AddressContent @@ addressContent)
      case _ => throw new Exception("Invalid address: Unsupported address type " + addressType)
    }
  }
}