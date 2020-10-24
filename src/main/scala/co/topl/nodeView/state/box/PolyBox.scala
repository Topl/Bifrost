package co.topl.nodeView.state.box

import co.topl.crypto.proposition.PublicKey25519Proposition
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

case class PolyBox(override val proposition: PublicKey25519Proposition,
                   override val nonce: Long,
                   override val value: Long) extends TokenBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "Poly"
}

object PolyBox {
  implicit val jsonEncoder: Encoder[PolyBox] =  (box: PolyBox) => TokenBox.jsonEncode(box).asJson

  implicit val jsonDecoder: Decoder[PolyBox] = ( c: HCursor ) =>
    TokenBox.jsonDecode(c).map {
      case (proposition, nonce, value) => PolyBox(proposition, nonce, value)
    }
}