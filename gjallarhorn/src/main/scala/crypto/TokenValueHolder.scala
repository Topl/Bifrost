package crypto

import io.circe.{Decoder, Encoder, HCursor}
import io.circe.syntax.EncoderOps

sealed abstract class TokenValueHolder(val quantity: Long)

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

case class SimpleValue(override val quantity: Long) extends TokenValueHolder(quantity)

object SimpleValue {
  val valueTypeString: String = "Simple"

  implicit val jsonEncoder: Encoder[SimpleValue] = (value: SimpleValue) =>
    Map(
      "type" -> valueTypeString.asJson,
      "quantity" -> value.quantity.asJson
    ).asJson

  implicit val jsonDecoder: Decoder[SimpleValue] = (hCursor: HCursor)  =>
    for {
      quantity <- hCursor.downField("quantity").as[Long]
    } yield {
      SimpleValue(quantity)
    }

}

case class AssetValue(override val quantity: Long,
                      assetCode: AssetCode,
                      securityRoot: SecurityRoot = SecurityRoot.empty,
                      metadata: Option[String] = None) extends TokenValueHolder(quantity)

object AssetValue {
  val valueTypeString: String = "Asset"

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
      quantity     <- c.downField("quantity").as[Long]
      assetCode    <- c.downField("assetCode").as[AssetCode]
      securityRoot <- c.downField("securityRoot").as[Option[String]]
      metadata     <- c.downField("metadata").as[Option[String]]
    } yield {
      val sr = securityRoot match {
        case Some(str) => SecurityRoot(str)
        case None      => SecurityRoot.empty
      }

      AssetValue(quantity, assetCode, sr, metadata)
    }
}