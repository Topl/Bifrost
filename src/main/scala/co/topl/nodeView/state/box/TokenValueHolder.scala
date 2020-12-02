package co.topl.nodeView.state.box

import co.topl.attestation.Address
import co.topl.nodeView.state.box.AssetValue.{AssetCode, SecurityRoot}
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58
import supertagged.TaggedType

sealed abstract class TokenValueHolder(val quantity: Long) extends BytesSerializable

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class SimpleValue(override val quantity: Long) extends TokenValueHolder(quantity) {

  override type M = SimpleValue

  override def serializer: BifrostSerializer[SimpleValue] = SimpleValue
}

object SimpleValue extends BifrostSerializer[SimpleValue] {
  implicit val jsonEncoder: Encoder[SimpleValue] = (value: SimpleValue) => value.quantity.asJson
  implicit val jsonDecoder: Decoder[SimpleValue] = Decoder.decodeString.map(str => SimpleValue(str.toLong))

  override def serialize(obj: SimpleValue, w: Writer): Unit =
    w.putULong(obj.quantity)

  override def parse(r: Reader): SimpleValue =
    SimpleValue(r.getULong())
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */
case class AssetValue(
  override val quantity: Long,
  assetCode: AssetCode,
  securityRoot: SecurityRoot,
  metadata: String
) extends TokenValueHolder(quantity) {

  require(assetCode.length == AssetValue.assetCodeSize, "Invalid assetCode")
  require(securityRoot.length == AssetValue.securityRootSize, "Invalid securityRoot")
  require(metadata.getBytes("utf8").length <= AssetValue.metadataLimit,
    "Metadata string must be less than 128 UTF-8 characters")

  override type M = AssetValue

  override def serializer: BifrostSerializer[AssetValue] = AssetValue
}

object AssetValue extends BifrostSerializer[AssetValue] {

  object AssetCode extends TaggedType[Array[Byte]]
  type AssetCode = AssetCode.Type

  object SecurityRoot extends TaggedType[Array[Byte]]
  type SecurityRoot = SecurityRoot.Type

  implicit val jsonEncoder: Encoder[AssetValue] = {
    (value: AssetValue) =>
      Map (
        "quantity" -> value.quantity.asJson,
        "assetCode" -> Base58.encode(value.assetCode).asJson,
        "securityRoot" -> Base58.encode(value.securityRoot).asJson,
        "metadata" -> value.metadata.asJson
      ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetValue] = (c: HCursor) =>
    for {
      quantity <- c.downField("quantity").as[Long]
      assetCode <- c.downField("assetCode").as[String].map(Base58.decode)
      securityRoot <- c.downField("securityRoot").as[String].map(Base58.decode)
      metadata <- c.downField("metadata").as[String]
    } yield {
      val ac = AssetCode @@ assetCode.getOrElse(throw new Exception("Unable to decode assetCode"))
      val sr = SecurityRoot @@ securityRoot.getOrElse(throw new Exception("Unable to decode securityRoot"))
      AssetValue(quantity, ac, sr, metadata)
    }

  // bytes (34 bytes for issuer Address + 8 bytes for asset nonce + 8 bytes for asset short name)
  val assetCodeSize: Int = Address.addressSize + 8 + 8
  val securityRootSize: Int = Blake2b256.DigestSize // 32 bytes
  val metadataLimit: Int = 128 // bytes of UTF-8 encoded string

  override def serialize(obj: AssetValue, w: Writer): Unit = {
    w.putULong(obj.quantity)
    w.putBytes(obj.assetCode)
    w.putBytes(obj.securityRoot)
    w.putByteString(obj.metadata)
  }

  override def parse(r: Reader): AssetValue = {
    val quantity = r.getULong()
    val assetCode = AssetCode @@ r.getBytes(assetCodeSize)
    val securityRoot = SecurityRoot @@ r.getBytes(securityRootSize)
    val metadata = r.getByteString()

    AssetValue(quantity, assetCode, securityRoot, metadata)
  }
}
