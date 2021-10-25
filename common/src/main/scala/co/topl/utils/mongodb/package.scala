package co.topl.utils

import cats.implicits._
import co.topl.attestation.{PublicKeyPropositionCurve25519, SignatureCurve25519}
import co.topl.modifier.box.AssetCode
import co.topl.utils.TimeProvider
import io.circe.{Decoder, DecodingFailure, Encoder, Json}
import io.circe.syntax.EncoderOps
import org.mongodb.scala.bson.Document
import co.topl.utils.codecs.implicits._
import io.circe.generic.semiauto._
import io.circe.parser._
import simulacrum.typeclass

import scala.collection.immutable.ListMap

package object mongodb {

  object models {

    case class AssetCodeDataModel(assetCodeVersion: Int, issuer: String, shortName: String)

    sealed trait TokenValueDataModel

    case class SimpleValueDataModel(quantity: String) extends TokenValueDataModel

    case class AssetValueDataModel(
      assetCode:    AssetCodeDataModel,
      quantity:     String,
      securityRoot: String,
      metadata:     Option[String]
    ) extends TokenValueDataModel

    case class BoxDataModel(boxType: String, id: String, nonce: String, evidence: String, value: TokenValueDataModel)

    case class BlockDataModel(
      id:              String,
      parentId:        String,
      timestamp:       Long,
      generatorBox:    BoxDataModel,
      publicKey:       String,
      signature:       String,
      height:          Long,
      difficulty:      Long,
      txRoot:          String,
      bloomFilter:     String,
      version:         Int,
      numTransactions: Int
    )

    case class BlockSummaryDataModel(id: String, height: Long)

    case class TransactionDataModel(
      block:           BlockSummaryDataModel,
      txType:          String,
      timestamp:       String,
      signatures:      ListMap[String, String],
      newBoxes:        List[BoxDataModel],
      data:            Option[String],
      from:            List[String],
      minting:         Boolean,
      txId:            String,
      boxesToRemove:   List[BoxDataModel],
      fee:             String,
      to:              List[String],
      propositionType: String
    )
  }

  @typeclass
  trait DocumentEncoder[T] {
    def asDocument(value: T): Document
  }

  @typeclass
  trait DocumentDecoder[T] {
    def fromDocument(document: Document): Either[String, T]
  }

  trait Codecs {
    import models._

    implicit val assetCodeModelJsonEncoder: Encoder[AssetCodeDataModel] = deriveEncoder

    implicit val assetCodeModelJsonDecoder: Decoder[AssetCodeDataModel] = deriveDecoder

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
                assetCode    <- json.get[AssetCodeDataModel]("assetCode")
                quantity     <- json.get[String]("quantity")
                securityRoot <- json.get[String]("securityRoot")
                metadata     <- json.get[Option[String]]("metadata")
              } yield AssetValueDataModel(assetCode, quantity, securityRoot, metadata)
            case _ => DecodingFailure("invalid token value type", List()).asLeft
          }

    implicit val boxDataModelJsonEncoder: Encoder[BoxDataModel] = deriveEncoder

    implicit val boxDataModelJsonDecoder: Decoder[BoxDataModel] = deriveDecoder

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

  object codecs extends Codecs

  object implicits extends DocumentDecoder.ToDocumentDecoderOps with DocumentEncoder.ToDocumentEncoderOps
}
