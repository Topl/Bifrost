package examples.bifrost.contract

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  *
  * @param terms
  * @param timestamp            the time at which the agreement was agreed upon
  * @param expirationTimestamp  timestamp to prevent parties from holding signatures until an advantageous date
  *                             for a previously agreed upon agreement
  */
case class Agreement(terms: AgreementTerms, timestamp: Long, expirationTimestamp: Long) {

  lazy val json: Json = Map(
    "terms" -> terms.json,
    "timestamp" -> timestamp.asJson,
    "expirationTimestamp" -> expirationTimestamp.asJson
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object Agreement {
  def validate(a: Agreement): Try[Unit] = Try {

    AgreementTerms.validate(a.terms)

    require(a.expirationTimestamp < System.currentTimeMillis) // TODO check this
    require(a.timestamp > 0)
    // require(a.timestamp > CONSTANTS.GENESIS_TIME)
    require(a.expirationTimestamp > a.timestamp)
  }
}

