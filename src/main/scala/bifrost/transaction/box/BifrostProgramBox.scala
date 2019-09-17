package bifrost.transaction.box

import java.util.UUID

import bifrost.transaction.account.PublicKeyNoncedBox
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import io.circe.Json
import io.circe.syntax._
import scorex.crypto.encode.Base58

abstract class BifrostProgramBox (override val proposition: PublicKey25519Proposition,
                                             override val nonce: Long,
                                             override val value: UUID) extends BifrostBox(proposition, nonce, value) {

  lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val typeOfBox: String = "BifrostProgramBox"

  lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "value" -> value.toString.asJson,
    "nonce" -> nonce.toString.asJson
  ).asJson

}
