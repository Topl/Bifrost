package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}

case class ArbitBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    SimpleValue
) extends TokenBox(evidence, nonce, value)

object ArbitBox {
  val typePrefix: BoxType = 1: Byte
  val typeString: String = "ArbitBox"

  implicit val identifier: Identifiable[ArbitBox] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }
}
