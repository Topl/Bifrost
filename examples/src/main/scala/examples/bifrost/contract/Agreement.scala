package examples.bifrost.contract

import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

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

