package co.topl.modifier.box

import co.topl.attestation.Address
import co.topl.codecs.binary.legacy.modifier.box.TokenValueHolderSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.Latin1Data

sealed abstract class TokenValueHolder(val quantity: Int128) extends BytesSerializable {

  @deprecated
  type M = TokenValueHolder

  @deprecated
  override def serializer: BifrostSerializer[TokenValueHolder] = TokenValueHolderSerializer
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class SimpleValue(override val quantity: Int128) extends TokenValueHolder(quantity)

object SimpleValue {
  val valueTypePrefix: Byte = 1: Byte
  val valueTypeString: String = "Simple"
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */
case class AssetValue(
  override val quantity: Int128,
  assetCode:             AssetCode,
  securityRoot:          SecurityRoot = SecurityRoot.empty,
  metadata:              Option[Latin1Data] = None
) extends TokenValueHolder(quantity)

object AssetValue {

  val valueTypePrefix: Byte = 2: Byte
  val valueTypeString: String = "Asset"

  // bytes (34 bytes for issuer Address + 8 bytes for asset short name)
  val assetCodeSize: Int = Address.addressSize + 8
  val metadataLimit: Byte = 127 // bytes of Latin-1 encoded string
}
