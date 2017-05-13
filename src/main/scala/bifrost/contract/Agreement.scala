package bifrost.contract

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  *
  * @param terms
  * @param contractEndTime  timestamp to prevent parties from holding signatures until an advantageous date
  *                             for a previously agreed upon agreement
  */
case class Agreement(terms: AgreementTerms, contractEndTime: Long) {

  lazy val json: Json = Map(
    "terms" -> terms.json,
    "expirationTimestamp" -> contractEndTime.asJson
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object Agreement {
  def validate(a: Agreement): Try[Unit] = Try {

    require(a.terms.pledge > 0)
    require(a.terms.xrate > 0)
    //TODO maybe validate functions here?

    // require(a.expirationTimestamp < System.currentTimeMillis) // TODO check this
    // require(a.timestamp > CONSTANTS.GENESIS_TIME)
  }
}

