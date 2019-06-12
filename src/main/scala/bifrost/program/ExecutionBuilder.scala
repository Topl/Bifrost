package bifrost.program

import bifrost.transaction.box.StateBox
import io.circe.{Decoder, Encoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

/**
  * @param core                     The JavaScript program split into state and functions
  * @param terms                    An AgreementTerms object that specifies the specific compensation terms
  * @param assetCode                The string identifier for this specific asset to be produced
  */
case class ExecutionBuilder(terms: AgreementTerms, assetCode: String, core: ProgramPreprocessor) {

  lazy val json: Json = Map(
    "terms" -> terms.json,
    "assetCode" -> assetCode.asJson,
    "core" -> core.json,
    "state" -> core.variables.asJson,
    "code" -> core.code.asJson
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object ExecutionBuilder {

  implicit val encodeAgreement: Encoder[ExecutionBuilder] = (a: ExecutionBuilder) => a.json

  implicit val decodeAgreement: Decoder[ExecutionBuilder] = (c: HCursor) => for {
    terms <- c.downField("terms").as[AgreementTerms]
    assetCode <- c.downField("assetCode").as[String]
    core <- c.downField("core").as[ProgramPreprocessor]
  } yield {
    ExecutionBuilder(terms, assetCode, core)
  }

  //def getStateBox: StateBox =

  def validate(a: ExecutionBuilder): Try[Unit] = Try {

  }
}

