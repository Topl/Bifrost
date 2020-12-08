package co.topl.nodeView.state.box

import co.topl.attestation.Address
import co.topl.nodeView.state.box.AssetValue.SecurityRoot
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable, Reader, Writer}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}
import scorex.crypto.hash.Blake2b256
import scorex.util.encode.Base58
import supertagged.TaggedType

import java.nio.charset.StandardCharsets

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
  metadata: Option[String]
) extends TokenValueHolder(quantity) {

  require(securityRoot.length == AssetValue.securityRootSize, "Invalid securityRoot")
  require(metadata.forall(_.getBytes(StandardCharsets.UTF_8).length <= AssetValue.metadataLimit),
    "Metadata string must be less than 128 UTF-8 characters")

  override type M = AssetValue

  override def serializer: BifrostSerializer[AssetValue] = AssetValue
}

object AssetValue extends BifrostSerializer[AssetValue] {

  object SecurityRoot extends TaggedType[Array[Byte]]
  type SecurityRoot = SecurityRoot.Type

  implicit val jsonEncoder: Encoder[AssetValue] = {
    (value: AssetValue) =>
      Map (
        "quantity" -> value.quantity.asJson,
        "assetCode" -> value.assetCode.asJson,
        "securityRoot" -> Base58.encode(value.securityRoot).asJson,
        "metadata" -> value.metadata.getOrElse("").asJson
      ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetValue] = (c: HCursor) =>
    for {
      quantity <- c.downField("quantity").as[Long]
      assetCode <- c.downField("assetCode").as[AssetCode]
      securityRoot <- c.downField("securityRoot").as[String].map(Base58.decode)
      metadata <- c.downField("metadata").as[Option[String]]
    } yield {
      val sr = SecurityRoot @@ securityRoot.getOrElse(throw new Exception("Unable to decode securityRoot"))
      AssetValue(quantity, assetCode, sr, metadata)
    }

  // bytes (34 bytes for issuer Address + 8 bytes for asset nonce + 8 bytes for asset short name)
  val assetCodeSize: Int = Address.addressSize + 8 + 8
  val securityRootSize: Int = Blake2b256.DigestSize // 32 bytes
  val metadataLimit: Int = 128 // bytes of UTF-8 encoded string

  override def serialize(obj: AssetValue, w: Writer): Unit = {
    w.putULong(obj.quantity)
    AssetCode.serialize(obj.assetCode, w)
    w.putBytes(obj.securityRoot)
    w.putOption(obj.metadata){ (writer, metadata) =>
      writer.putByteString(metadata)
    }
  }

  override def parse(r: Reader): AssetValue = {
    val quantity = r.getULong()
    val assetCode = AssetCode.parse(r)
    val securityRoot = SecurityRoot @@ r.getBytes(securityRootSize)
    val metadata: Option[String] = r.getOption {
      r.getByteString()
    }

    AssetValue(quantity, assetCode, securityRoot, metadata)
  }
}
