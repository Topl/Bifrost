package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import co.topl.utils.HasName
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

case class PolyBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    SimpleValue
) extends TokenBox(evidence, nonce, value, PolyBox.boxTypePrefix)

object PolyBox {
  val boxTypePrefix: BoxType = 2: Byte
  val boxTypeString: String = "PolyBox"

  implicit val name: HasName[PolyBox] = HasName.instance(() => boxTypeString)

  implicit val jsonEncoder: Encoder[PolyBox] = (box: PolyBox) => Box.jsonEncode[SimpleValue, PolyBox](box).asJson

  implicit val jsonDecoder: Decoder[PolyBox] = (c: HCursor) =>
    Box.jsonDecode[SimpleValue](c).map { case (evidence, nonce, value) =>
      PolyBox(evidence, nonce, value)
    }
}
