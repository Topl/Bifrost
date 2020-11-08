package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, HCursor }

case class ArbitBox( override val evidence: Evidence,
                     override val nonce: Box.Nonce,
                     override val value: TokenBox.Value
                   ) extends TokenBox(evidence, nonce, value, ArbitBox.boxTypePrefix)

object ArbitBox {
  val boxTypePrefix: BoxType = 1: Byte

  implicit val jsonEncoder: Encoder[ArbitBox] =  (box: ArbitBox) => Box.jsonEncode(box).asJson

  implicit val jsonDecoder: Decoder[ArbitBox] = ( c: HCursor ) =>
    Box.jsonDecode(c).map {
      case (proposition, nonce, value) => ArbitBox(proposition, nonce, value)
    }
}
