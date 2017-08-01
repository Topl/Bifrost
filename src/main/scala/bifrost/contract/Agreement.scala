package bifrost.contract

import bifrost.contract.modules.BaseModuleWrapper
import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

/**
  *
  * @param terms                    an AgreementTerms object that specifies the specific compensation terms
  * @param assetCode                the string identifier for this specific asset to be produced
  */
case class Agreement(terms: AgreementTerms, assetCode: String, core: BaseModuleWrapper) {

  lazy val json: Json = Map(
    "terms" -> terms.json,
    "assetCode" -> assetCode.asJson,
    "core" -> core.json
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object Agreement {

  def validate(a: Agreement): Try[Unit] = Try {
    require(a.terms.pledge > 0)
    require(a.terms.xrate > 0)
  }
}

