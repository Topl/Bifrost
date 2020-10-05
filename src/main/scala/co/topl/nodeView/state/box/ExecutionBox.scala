package co.topl.nodeView.state.box

import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor, Json }

case class ExecutionBox( override val proposition: PublicKey25519Proposition,
                         override val nonce      : Long,
                         override val value      : ProgramId,
                         stateBoxIds             : Seq[ProgramId],
                         codeBoxIds              : Seq[ProgramId]
                        ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "ExecutionBox"
}

object ExecutionBox {

  implicit val jsonEncoder: Encoder[ExecutionBox] = (box: ExecutionBox) =>
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "programId" -> box.value.toString.asJson,
      "stateBoxIds" -> box.stateBoxIds.asJson,
      "codeBoxIds" -> box.codeBoxIds.asJson,
      "nonce" -> box.nonce.toString.asJson,
      ).asJson

  implicit val jsonDecoder: Decoder[ExecutionBox] = (c: HCursor) =>
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      programId <- c.downField("programId").as[ProgramId]
      nonce <- c.downField("nonce").as[Long]
      stateBoxIds <- c.downField("stateBoxIds").as[Seq[ProgramId]]
      codeBoxIds <- c.downField("codeBoxIds").as[Seq[ProgramId]]
    } yield {
      ExecutionBox(proposition, nonce, programId, stateBoxIds, codeBoxIds)
    }
}
