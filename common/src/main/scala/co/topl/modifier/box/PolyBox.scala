package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}

case class PolyBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    SimpleValue
) extends TokenBox(evidence, nonce, value)

object PolyBox {
  val typePrefix: BoxType = 2: Byte
  val typeString: String = "PolyBox"

  implicit val identifier: Identifiable[PolyBox] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }
}
