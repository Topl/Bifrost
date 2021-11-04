package co.topl.modifier.box

import co.topl.attestation.Address
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.Int128
import co.topl.utils.StringDataTypes.{Base58Data, Latin1Data}
import co.topl.utils.codecs.binary.legacy.modifier.box.TokenValueHolderSerializer
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, BytesSerializable}
import co.topl.utils.codecs._
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

sealed abstract class TokenValueHolder(val quantity: Int128) {

  def serializer: BifrostSerializer[TokenValueHolder] = TokenValueHolderSerializer
}

object TokenValueHolder {

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
}

/* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */ /* ----------------- */

case class SimpleValue(override val quantity: Int128) extends TokenValueHolder(quantity)

object SimpleValue {
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
    } yield SimpleValue(quantity)
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

  implicit val jsonEncoder: Encoder[AssetValue] = { (value: AssetValue) =>
    Map(
      "type"         -> valueTypeString.asJson,
      "quantity"     -> value.quantity.asJson,
      "assetCode"    -> value.assetCode.asJson,
      "securityRoot" -> value.securityRoot.asJson,
      "metadata"     -> value.metadata.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetValue] = (c: HCursor) =>
    for {
      quantity     <- c.get[Int128]("quantity")
      assetCode    <- c.downField("assetCode").as[AssetCode]
      securityRoot <- c.downField("securityRoot").as[Option[Base58Data]]
      metadata     <- c.downField("metadata").as[Option[Latin1Data]]
    } yield {
      val sr = securityRoot match {
        case Some(data) => data.value.decodeTransmitted[SecurityRoot].getOrThrow()
        case None       => SecurityRoot.empty
      }

      AssetValue(quantity, assetCode, sr, metadata)
    }
}
