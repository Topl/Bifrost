package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class ExecutionBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    ProgramId,
  stateBoxIds:           Seq[ProgramId],
  codeBoxIds:            Seq[ProgramId]
) extends ProgramBox(evidence, nonce, value)

object ExecutionBox {
  val typePrefix: BoxType = 11: Byte
  val typeString: String = "ExecutionBox"

  implicit val identifier: Identifiable[ExecutionBox] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  implicit val jsonEncoder: Encoder[ExecutionBox] = { box: ExecutionBox =>
    (Box.jsonEncode[ProgramId, ExecutionBox](box) ++ Map(
      "stateBoxIds" -> box.stateBoxIds.asJson,
      "codeBoxIds"  -> box.codeBoxIds.asJson
    )).asJson
  }

  implicit val jsonDecoder: Decoder[ExecutionBox] = (c: HCursor) =>
    for {
      b           <- Box.jsonDecode[ProgramId](c)
      stateBoxIds <- c.downField("stateBoxIds").as[Seq[ProgramId]]
      codeBoxIds  <- c.downField("codeBoxIds").as[Seq[ProgramId]]
    } yield {
      val (evidence, nonce, programId) = b
      ExecutionBox(evidence, nonce, programId, stateBoxIds, codeBoxIds)
    }
}
