package co.topl.utils.mongodb

import cats.implicits._
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.generic.semiauto.{deriveDecoder, deriveEncoder}
import io.circe.parser.parse
import org.mongodb.scala.bson.Document
import co.topl.utils.mongodb.models._
import io.circe.syntax._

trait Codecs {

  implicit val simpleValueDataModelJsonEncoder: Encoder[SimpleValueDataModel] = deriveEncoder

  implicit val simpleValueDataModelJsonDecoder: Decoder[SimpleValueDataModel] = deriveDecoder

  implicit val assetValueDataModelJsonEncoder: Encoder[AssetValueDataModel] = deriveEncoder

  implicit val assetValueDataModelJsonDecoder: Decoder[AssetValueDataModel] = deriveDecoder

  implicit val tokenValueDataModelJsonEncoder: Encoder[TokenValueDataModel] = {
    case SimpleValueDataModel(quantity: String) =>
      Json.obj("type" -> "Simple".asJson, "quantity" -> quantity.asJson)
    case AssetValueDataModel(assetCode, quantity, securityRoot, metadata) =>
      Json.obj(
        "type"         -> "Asset".asJson,
        "assetCode"    -> assetCode.asJson,
        "quantity"     -> quantity.asJson,
        "securityRoot" -> securityRoot.asJson,
        "metadata"     -> metadata.asJson
      )
  }

  implicit val tokenValueDataModelJsonDecoder: Decoder[TokenValueDataModel] =
    json =>
      json
        .get[String]("type")
        .flatMap {
          case "Simple" => json.get[String]("quantity").map(SimpleValueDataModel)
          case "Asset" =>
            for {
              assetCode    <- json.get[String]("assetCode")
              quantity     <- json.get[String]("quantity")
              securityRoot <- json.get[String]("securityRoot")
              metadata     <- json.get[Option[String]]("metadata")
            } yield AssetValueDataModel(assetCode, quantity, securityRoot, metadata)
          case _ => DecodingFailure("invalid token value type", List()).asLeft
        }

  implicit val boxDataModelJsonEncoder: Encoder[TokenBoxDataModel] = deriveEncoder

  implicit val boxDataModelJsonDecoder: Decoder[TokenBoxDataModel] = deriveDecoder

  implicit val blockDataModelJsonEncoder: Encoder[BlockDataModel] = deriveEncoder

  implicit val blockDataModelJsonDecoder: Decoder[BlockDataModel] = deriveDecoder

  implicit val blockSummaryDataModelJsonEncoder: Encoder[BlockSummaryDataModel] = deriveEncoder

  implicit val blockSummaryDataModelJsonDecoder: Decoder[BlockSummaryDataModel] = deriveDecoder

  implicit val transactionDataModelJsonEncoder: Encoder[TransactionDataModel] = deriveEncoder

  implicit val transactionDataModelJsonDecoder: Decoder[TransactionDataModel] = deriveDecoder

  implicit def jsonEncoderAsDocumentEncoder[T: Encoder]: DocumentEncoder[T] =
    value => Document(value.asJson.noSpaces)

  implicit def jsonDecoderAsDocumentDecoder[T: Decoder]: DocumentDecoder[T] =
    document =>
      for {
        jsonObj <- parse(document.toJson()).leftMap(_.message)
        value   <- Decoder[T].decodeJson(jsonObj).leftMap(_.message)
      } yield value
}
