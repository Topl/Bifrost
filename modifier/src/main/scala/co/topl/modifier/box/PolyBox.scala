package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

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

  implicit val jsonEncoder: Encoder[PolyBox] = (box: PolyBox) => Box.jsonEncode[SimpleValue, PolyBox](box).asJson

  implicit val jsonDecoder: Decoder[PolyBox] = (c: HCursor) =>
    Box.jsonDecode[SimpleValue](c).map { case (evidence, nonce, value) =>
      PolyBox(evidence, nonce, value)
    }
}
