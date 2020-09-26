package bifrost.modifier.box

import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs
import io.circe.syntax._
import io.circe.{Decoder, HCursor, Json}
import scorex.util.encode.Base58

//TODO change codeBoxIds to codeBoxUUIDs
case class ExecutionBox(override val proposition: PublicKey25519Proposition,
                        override val nonce: Long,
                        override val value: UUID,
                        stateBoxUUIDs: Seq[UUID], //List of uuids of state boxes from ProgramBoxRegistry
                        codeBoxIds: Seq[Array[Byte]]
                        ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "ExecutionBox"

  override lazy val id: Array[Byte] = ExecutionBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "uuid" -> value.asJson,
    "stateBoxUUIDs" -> stateBoxUUIDs.asJson,
    "codeBoxIds" -> codeBoxIds.map(cb => Base58.encode(cb)).asJson,
    "nonce" -> nonce.toString.asJson,
  ).asJson

}

object ExecutionBox {

  def idFromBox[proposition <: PublicKey25519Proposition](prop: proposition, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "execution".getBytes ++ Longs.toByteArray(nonce))

  // TODO: Jing - Check if this is used anywhere
  implicit val decodeCodeBox: Decoder[ExecutionBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    uuid <- c.downField("uuid").as[UUID]
    stateBoxUUIDs <- c.downField("stateBoxUUIDs").as[Seq[UUID]]
    nonce <- c.downField("nonce").as[Long]
    codeBoxIds <- c.downField("codeBoxIds").as[Seq[String]]
  } yield {
//      val preparedPubKey = proposition.map(t => Base58.decode(t).get).toSet
//      val prop = MofNProposition(1, preparedPubKey)
    val preparedPubKey = Base58.decode(proposition).get
    val prop = PublicKey25519Proposition(preparedPubKey)
    val codeBoxes: Seq[Array[Byte]] = codeBoxIds.map(cb => Base58.decode(cb).get)
    ExecutionBox(prop, nonce, uuid, stateBoxUUIDs, codeBoxes)
  }
}
