package co.topl.nodeView.state.box

import co.topl.crypto.{PrivateKey25519, ProofOfKnowledgeProposition, Secret}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, HCursor}

case class ArbitBox(override val proposition: ProofOfKnowledgeProposition[_ <: Secret],
                    override val nonce: Long,
                    override val value: Long) extends TokenBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "Arbit"
}

object ArbitBox {
  implicit val jsonEncoder: Encoder[ArbitBox] =  (box: ArbitBox) => TokenBox.jsonEncode(box).asJson

  implicit val jsonDecoder: Decoder[ArbitBox] = ( c: HCursor ) =>
    TokenBox.jsonDecode(c).map {
      case (proposition, nonce, value) => ArbitBox(proposition, nonce, value)
    }
}
