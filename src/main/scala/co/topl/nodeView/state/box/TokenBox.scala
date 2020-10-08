package co.topl.nodeView.state.box

import co.topl.crypto.FastCryptographicHash
import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import com.google.common.primitives.Longs
import io.circe.syntax.EncoderOps
import io.circe.{DecodingFailure, Encoder, HCursor, Json}

 abstract class TokenBox(override val proposition: PublicKey25519Proposition,
                         override val nonce: Long,
                         override val value: Long
                        ) extends Box(proposition, nonce, value) {
  self =>

  lazy val id: BoxId = TokenBox.idFromBox(self)

}


object TokenBox {
  def idFromBox[PKP <: PublicKey25519Proposition] (box: TokenBox ): BoxId = {
    val hashBytes = FastCryptographicHash(
      box.proposition.pubKeyBytes ++
        box.typeOfBox.getBytes ++
        Longs.toByteArray(box.nonce))

    BoxId(hashBytes)
  }

  def jsonEncode(box: TokenBox): Map[String, Json] =
    Map(
      "id" -> box.id.toString.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.toString.asJson,
      "value" -> box.value.toString.asJson,
      "nonce" -> box.nonce.toString.asJson
    )

  def jsonDecode(c: HCursor): Either[DecodingFailure, (PublicKey25519Proposition, Long, Long)] =
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      value <- c.downField("value").as[Long]
      nonce <- c.downField("issuer").as[Long]
    } yield {
      (proposition, nonce, value)
    }
}