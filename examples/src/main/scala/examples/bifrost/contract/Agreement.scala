package examples.bifrost.contract

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

import scala.util.Try

/**
  * Created by cykoz on 4/13/17.
  */
case class Agreement(parties: IndexedSeq[PublicKey25519Proposition],
                     terms: AgreementTerms,
                     nonce: Long,
                     timestamp: Long,
                     expirationTimestamp: Long) {

  lazy val json: Json = Map(
    "parties" -> Array( parties.map(p => Base58.encode(p.pubKeyBytes)) ).asJson,
    "terms" -> terms.json,
    "nonce" -> nonce.asJson,
    "timestamp" -> timestamp.asJson,
    "expirationTimestamp" -> expirationTimestamp.asJson
  ).asJson

  override def toString: String = s"Agreement(${json.toString})"

}

object Agreement {
  def validate(a: Agreement): Try[Unit] = Try {

    AgreementTerms.validate(a.terms)

    require(a.parties.length == 3)
    require(a.expirationTimestamp < System.currentTimeMillis) // TODO check this
    require(a.timestamp > 0)
    // require(a.timestamp > CONSTANTS.GENESIS_TIME)
    require(a.nonce > 0)
    require(a.expirationTimestamp > a.timestamp)
  }
}

