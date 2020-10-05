package co.topl.nodeView.state.box

import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, HCursor }

case class PolyBox(override val proposition: PublicKey25519Proposition,
                   override val nonce: Long,
                   override val value: Long) extends TokenBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "Poly"
}

object PolyBox {
  implicit val jsonEncoder: Encoder[PolyBox] = { box: PolyBox =>
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
      ).asJson
  }

  implicit val jsonDecoder: Decoder[PolyBox] = ( c: HCursor ) =>
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      value <- c.downField("value").as[Long]
      nonce <- c.downField("issuer").as[Long]
    } yield {
      PolyBox(proposition, nonce, value)
    }
}