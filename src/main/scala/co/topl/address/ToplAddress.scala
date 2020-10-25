package co.topl.address

import co.topl.address.ToplAddress.{AddressContent, AddressTypePrefix}
import co.topl.address.serialization.ToplAddressSerializer
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import com.google.common.primitives.Ints
import supertagged.TaggedType

abstract class ToplAddress(val addressTypePrefix: AddressTypePrefix, val content: AddressContent) extends BytesSerializable {

  type M = ToplAddress

  lazy val addressBytes: Array[Byte] = addressTypePrefix +: content

  def equals(obj: Any): Boolean

  override def hashCode(): Int = Ints.fromByteArray(content)

  override def serializer: BifrostSerializer[ToplAddress] = ToplAddressSerializer

}

object ToplAddress {
  type AddressTypePrefix = Byte

  object AddressContent extends TaggedType[Array[Byte]]
  type AddressContent = AddressContent.Type

  val addressContentLength = 32 //bytes
}

