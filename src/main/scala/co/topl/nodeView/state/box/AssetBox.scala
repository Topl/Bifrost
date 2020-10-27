package co.topl.nodeView.state.box

import co.topl.attestation.proposition.PublicKey25519Proposition
import io.circe.syntax._
import io.circe.{ Decoder, Encoder, HCursor }

case class AssetBox ( override val proposition: PublicKey25519Proposition,
                      override val nonce      : Long,
                      override val value      : Long,
                      assetCode               : String,
                      issuer                  : PublicKey25519Proposition,
                      data                    : String
                    ) extends TokenBox(proposition, nonce, value) {

  override lazy val typeOfBox: String = "Asset"

}

object AssetBox {
  implicit val jsonEncoder: Encoder[AssetBox] = { box: AssetBox =>
    (TokenBox.jsonEncode(box) ++ Map(
      "issuer" -> box.issuer.asJson,
      "data" -> box.data.asJson,
      "nonce" -> box.nonce.toString.asJson)
      ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetBox] = ( c: HCursor ) =>
    for {
      b <- TokenBox.jsonDecode(c)
      assetCode <- c.downField("assetCode").as[String]
      issuer <- c.downField("issuer").as[PublicKey25519Proposition]
      data <- c.downField("data").as[String]
    } yield {
      val (proposition, nonce, value) = b
      AssetBox(proposition, nonce, value, assetCode, issuer, data)
    }
}
