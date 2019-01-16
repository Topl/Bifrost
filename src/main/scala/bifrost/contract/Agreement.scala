package bifrost.contract

import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

/**
  *
  * @param terms                    an AgreementTerms object that specifies the specific compensation terms
  * @param assetCode                the string identifier for this specific asset to be produced
  */
case class Agreement(terms: AgreementTerms, assetCode: String, core: ProgramPreprocessor) {

  lazy val json: Json = Map(
    "terms" -> terms.json,
    "assetCode" -> assetCode.asJson,
    "core" -> core.json
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object Agreement {

  implicit val encodeAgreement: Encoder[Agreement] = (a: Agreement) => a.json

  implicit val decodeAgreement: Decoder[Agreement] = (c: HCursor) => for {
    terms <- c.downField("terms").as[AgreementTerms]
    assetCode <- c.downField("assetCode").as[String]
    core <- c.downField("core").as[ProgramPreprocessor]
  } yield {
    Agreement(terms, assetCode, core)
  }

  def validate(a: Agreement): Try[Unit] = Try {

  }
}

