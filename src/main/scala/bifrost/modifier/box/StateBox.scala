package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.crypto.encode.Base58

case class StateBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: UUID,
                    state: Json //  JSON representation of JS Variable Declarations
                    ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "StateBox"

  override lazy val id: Array[Byte] = StateBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "uuid" -> value.asJson,
    "state" -> state.asJson,
    "nonce" -> nonce.toString.asJson,
  ).asJson
}

object StateBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "state".getBytes ++ Longs.toByteArray(nonce))

  // TODO: Jing - Check if this is used anywhere
  implicit val decodeStateBox: Decoder[StateBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("uuid").as[UUID]
    state <- c.downField("state").as[Json]
    nonce <- c.downField("nonce").as[Long]
  } yield {
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    StateBox(prop, nonce, value, state)
  }
}
