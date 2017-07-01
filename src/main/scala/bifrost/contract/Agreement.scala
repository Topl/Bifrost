package bifrost.contract

import java.time.Instant

import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

/**
  *
  * @param terms                    an AgreementTerms object that specifies the specific compensation terms
  * @param assetCode                the string identifier for this specific asset to be produced
  * @param contractEffectiveTime    timestamp that specifies when the contract becomes interactable
  * @param contractExpirationTime   timestamp that specifies when the contract expires and ceases to be updateable
  */
case class Agreement(terms: AgreementTerms, assetCode: String, contractEffectiveTime: Long, contractExpirationTime: Long) {

  lazy val json: Json = Map(
    "terms" -> terms.json,
    "assetCode" -> assetCode.asJson,
    "contractEffectiveTime" -> contractEffectiveTime.asJson,
    "contractExpirationTime" -> contractExpirationTime.asJson
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object Agreement {

  def validate(a: Agreement): Try[Unit] = Try {

    require(a.terms.pledge > 0)
    require(a.terms.xrate > 0)

    require(a.contractExpirationTime > Instant.now.toEpochMilli)
    require(a.contractEffectiveTime > Instant.now.toEpochMilli)
    require(a.contractExpirationTime > a.contractEffectiveTime)

  }
}

