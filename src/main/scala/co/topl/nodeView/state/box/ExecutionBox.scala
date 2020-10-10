package co.topl.nodeView.state.box

import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class ExecutionBox( override val proposition: PublicKey25519Proposition,
                         override val nonce      : Long,
                         override val value      : ProgramId,
                         stateBoxIds             : Seq[ProgramId],
                         codeBoxIds              : Seq[ProgramId]
                        ) extends ProgramBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "ExecutionBox"
}

object ExecutionBox {

  implicit val jsonEncoder: Encoder[ExecutionBox] = { box: ExecutionBox =>
    (ProgramBox.jsonEncode(box) ++ Map(
      "stateBoxIds" -> box.stateBoxIds.asJson,
      "codeBoxIds" -> box.codeBoxIds.asJson
    )).asJson
  }

  implicit val jsonDecoder: Decoder[ExecutionBox] = ( c: HCursor ) =>
    for {
      b <- ProgramBox.jsonDecode(c)
      stateBoxIds <- c.downField("stateBoxIds").as[Seq[ProgramId]]
      codeBoxIds <- c.downField("codeBoxIds").as[Seq[ProgramId]]
    } yield {
      val (proposition, nonce, programId) = b
      ExecutionBox(proposition, nonce, programId, stateBoxIds, codeBoxIds)
    }
}
