package bifrost.modifier.box

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.state.ProgramId
import com.google.common.primitives.Longs
import io.circe.syntax._
import io.circe.{ Decoder, HCursor, Json }
import scorex.crypto.encode.Base58

case class ExecutionBox( override val proposition: PublicKey25519Proposition,
                         override val nonce      : Long,
                         override val value      : ProgramId,
                         stateBoxIds             : Seq[ProgramId], //List of program ids of state boxes from ProgramBoxRegistry
                         codeBoxIds              : Seq[ProgramId]
                        ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "ExecutionBox"

  override lazy val id: Array[Byte] = ExecutionBox.idFromBox(proposition, nonce)

  override lazy val json: Json = Map(
    "id" -> Base58.encode(id).asJson,
    "type" -> typeOfBox.asJson,
    "proposition" -> Base58.encode(proposition.pubKeyBytes).asJson,
    "programId" -> value.toString.asJson,
    "stateBoxIds" -> stateBoxIds.map(_.toString).asJson,
    "codeBoxIds" -> codeBoxIds.map(_.toString).asJson,
    "nonce" -> nonce.toString.asJson,
    ).asJson
}

object ExecutionBox {

  def idFromBox[P <: PublicKey25519Proposition](prop: P, nonce: Long): Array[Byte] =
    FastCryptographicHash(prop.pubKeyBytes ++ "execution".getBytes ++ Longs.toByteArray(nonce))

  implicit val decodeExecBox: Decoder[ExecutionBox] = (c: HCursor) => for {
    proposition <- c.downField("proposition").as[String]
    value <- c.downField("programId").as[String]
    nonce <- c.downField("nonce").as[Long]
    stateBoxIds <- c.downField("stateBoxIds").as[Seq[String]]
    codeBoxIds <- c.downField("codeBoxIds").as[Seq[String]]
  } yield {
    val prop = PublicKey25519Proposition(proposition).get
    val progId = ProgramId(value).get
    val stateIds: Seq[ProgramId] = stateBoxIds.map(sb => ProgramId(sb).get)
    val codeIds: Seq[ProgramId] = codeBoxIds.map(cb => ProgramId(cb).get)
    ExecutionBox(prop, nonce, progId, stateIds, codeIds)
  }
}
