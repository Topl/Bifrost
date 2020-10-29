package bifrost.modifier.box

import bifrost.modifier.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

/** Created by cykoz on 5/15/2017.
  */

class TokenBox(override val proposition: PublicKey25519Proposition, override val nonce: Long, override val value: Long)
    extends Box(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "TokenBox"

  lazy val json: Json = Map(
    "id"          -> Base58.encode(id).asJson,
    "type"        -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value"       -> value.toString.asJson,
    "nonce"       -> nonce.toString.asJson
  ).asJson
}
