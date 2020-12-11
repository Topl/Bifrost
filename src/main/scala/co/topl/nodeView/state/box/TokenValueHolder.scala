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

sealed abstract class TokenValueHolder(val quantity: Long) extends BytesSerializable {
  override type M = TokenValueHolder

  override def serializer: BifrostSerializer[TokenValueHolder] = TokenValueHolder
}

object TokenValueHolder extends BifrostSerializer[TokenValueHolder] {

  implicit val jsonEncoder: Encoder[TokenValueHolder] = {
    case v: SimpleValue => SimpleValue.jsonEncoder(v)
    case v: AssetValue  => AssetValue.jsonEncoder(v)
    case _              => throw new Error(s"No matching encoder found")
  }

  implicit val jsonDecoder: Decoder[TokenValueHolder] = { c: HCursor =>
    c.downField("type").as[String].map {
      case SimpleValue.valueTypeString => SimpleValue.jsonDecoder(c)
      case AssetValue.valueTypeString  => AssetValue.jsonDecoder(c)
    } match {
      case Right(v) => v
      case Left(ex) => throw ex
    }
  }

  override def serialize(obj: TokenValueHolder, w: Writer): Unit = {
    obj match {
      case obj: SimpleValue =>
        w.put(SimpleValue.valueTypePrefix)
        SimpleValue.serialize(obj, w)

      case obj: AssetValue =>
        w.put(AssetValue.valueTypePrefix)
        AssetValue.serialize(obj, w)

      case _ => throw new Exception("Unanticipated TokenValueType type")
    }
  }

  override def parse(r: Reader): TokenValueHolder = {
    r.getByte() match {
      case SimpleValue.valueTypePrefix => SimpleValue.parse(r)
      case AssetValue.valueTypePrefix  => AssetValue.parse(r)
      case _                           => throw new Exception("Unanticipated Box Type")
    }
  }
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class SimpleValue(override val quantity: Long) extends TokenValueHolder(quantity)

object SimpleValue extends BifrostSerializer[SimpleValue] {
  val valueTypePrefix: Byte = 1: Byte
  val valueTypeString: String = "Simple"

  implicit val jsonEncoder: Encoder[SimpleValue] = { (value: SimpleValue) =>
    Map(
      "type"     -> valueTypeString.asJson,
      "quantity" -> value.quantity.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[SimpleValue] = (c: HCursor) =>
    for {
      quantity <- c.downField("quantity").as[Long]
    } yield {
      SimpleValue(quantity)
    }

  override def serialize(obj: SimpleValue, w: Writer): Unit =
    w.putULong(obj.quantity)

  override def parse(r: Reader): SimpleValue =
    SimpleValue(r.getULong())
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */
case class AssetValue(
  override val quantity: Long,
  assetCode:             AssetCode,
  securityRoot:          SecurityRoot = AssetValue.emptySecurityRoot,
  metadata:              Option[String] = None
) extends TokenValueHolder(quantity) {

  require(securityRoot.length == AssetValue.securityRootSize, "Invalid securityRoot")
  require(metadata.forall(_.getBytes(StandardCharsets.UTF_8).length <= AssetValue.metadataLimit),
          "Metadata string must be less than 128 UTF-8 characters"
  )

}

object AssetValue extends BifrostSerializer[AssetValue] {

  object SecurityRoot extends TaggedType[Array[Byte]]
  type SecurityRoot = SecurityRoot.Type

  val valueTypePrefix: Byte = 2: Byte
  val valueTypeString: String = "Asset"

  // bytes (34 bytes for issuer Address + 8 bytes for asset nonce + 8 bytes for asset short name)
  val assetCodeSize: Int = Address.addressSize + 8 + 8
  val securityRootSize: Int = Blake2b256.DigestSize // 32 bytes
  val metadataLimit: Int = 128 // bytes of UTF-8 encoded string
  val emptySecurityRoot: SecurityRoot = SecurityRoot @@ Array.fill(securityRootSize)(0: Byte)

  implicit val jsonEncoder: Encoder[AssetValue] = { (value: AssetValue) =>
    Map(
      "type"         -> valueTypeString.asJson,
      "quantity"     -> value.quantity.asJson,
      "assetCode"    -> value.assetCode.asJson,
      "securityRoot" -> Base58.encode(value.securityRoot).asJson,
      "metadata"     -> value.metadata.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetValue] = (c: HCursor) =>
    for {
      quantity     <- c.downField("quantity").as[Long]
      assetCode    <- c.downField("assetCode").as[AssetCode]
      securityRoot <- c.downField("securityRoot").as[Option[String]]
      metadata     <- c.downField("metadata").as[Option[String]]
    } yield {
      val sr = SecurityRoot @@ (securityRoot match {
        case Some(str) => Base58.decode(str).getOrElse(throw new Exception("Unable to decode securityRoot"))
        case None      => AssetValue.emptySecurityRoot
      })

      AssetValue(quantity, assetCode, sr, metadata)
    }

  override def serialize(obj: AssetValue, w: Writer): Unit = {
    w.putULong(obj.quantity)
    AssetCode.serialize(obj.assetCode, w)
    w.putBytes(obj.securityRoot)
    w.putOption(obj.metadata) { (writer, metadata) =>
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
