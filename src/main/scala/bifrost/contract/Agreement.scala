package bifrost.contract

import java.time.Instant

import io.circe.{Decoder, HCursor, Json}
import io.circe.syntax._

import scala.util.Try

/**
  *
  * @param terms
  * @param contractExpirationTime  timestamp to prevent parties from holding signatures until an advantageous date
  *                             for a previously agreed upon agreement
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
    //TODO maybe validate share/fulfill functions

    require(a.contractExpirationTime > Instant.now.toEpochMilli)
    require(a.contractEffectiveTime > Instant.now.toEpochMilli)
    require(a.contractExpirationTime > a.contractEffectiveTime)

  }
}

