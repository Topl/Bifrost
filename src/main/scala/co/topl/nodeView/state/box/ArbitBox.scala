package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import co.topl.utils.HasName
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

case class ArbitBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    SimpleValue
) extends TokenBox(evidence, nonce, value, ArbitBox.boxTypePrefix)

object ArbitBox {
  val boxTypePrefix: BoxType = 1: Byte
  val boxTypeString: String = "ArbitBox"

  implicit val name: HasName[ArbitBox] = HasName.instance(() => boxTypeString)

  implicit val jsonEncoder: Encoder[ArbitBox] = (box: ArbitBox) => Box.jsonEncode[SimpleValue, ArbitBox](box).asJson

  implicit val jsonDecoder: Decoder[ArbitBox] = (c: HCursor) =>
    Box.jsonDecode[SimpleValue](c).map { case (evidence, nonce, value) =>
      ArbitBox(evidence, nonce, value)
    }
}
