package bifrost.modifier.box

import java.util.UUID

import bifrost.modifier.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

class ProgramBox(override val proposition: PublicKey25519Proposition, override val nonce: Long, override val value: UUID)
    extends Box(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "ProgramBox"

  lazy val json: Json = Map(
    "id"          -> Base58.encode(id).asJson,
    "type"        -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value"       -> value.toString.asJson,
    "nonce"       -> nonce.toString.asJson
  ).asJson
}
