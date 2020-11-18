package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

case class PolyBox(override val evidence: Evidence,
                   override val nonce: Box.Nonce,
                   override val value: TokenBox.Value
                  ) extends TokenBox(evidence, nonce, value, PolyBox.boxTypePrefix)

object PolyBox {
  val boxTypePrefix: BoxType = 2: Byte

  implicit val jsonEncoder: Encoder[PolyBox] =  (box: PolyBox) => Box.jsonEncode(box).asJson

  implicit val jsonDecoder: Decoder[PolyBox] = ( c: HCursor ) =>
    Box.jsonDecode[TokenBox.Value](c).map {
      case (evidence, nonce, value) => PolyBox(evidence, nonce, value)
    }
}