package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

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

  implicit val jsonEncoder: Encoder[ArbitBox] = (box: ArbitBox) => Box.jsonEncode[SimpleValue, ArbitBox](box).asJson

  implicit val jsonDecoder: Decoder[ArbitBox] = (c: HCursor) =>
    Box.jsonDecode[SimpleValue](c).map { case (evidence, nonce, value) =>
      ArbitBox(evidence, nonce, value)
    }
}
