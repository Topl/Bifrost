package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}

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
}
