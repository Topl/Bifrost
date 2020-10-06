package co.topl.nodeView.state.box

import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import io.circe.syntax.EncoderOps
import io.circe.{ Decoder, Encoder, HCursor }

case class ArbitBox(override val proposition: PublicKey25519Proposition,
                    override val nonce: Long,
                    override val value: Long) extends TokenBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "Arbit"
}

object ArbitBox {
  implicit val jsonEncoder: Encoder[ArbitBox] = { box: ArbitBox =>
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
    ).asJson
  }

  implicit val jsonDecoder: Decoder[ArbitBox] = ( c: HCursor ) =>
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      value <- c.downField("value").as[Long]
      nonce <- c.downField("issuer").as[Long]
    } yield {
      ArbitBox(proposition, nonce, value)
    }
}
