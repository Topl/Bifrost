package co.topl.nodeView.state.box

import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
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
  implicit val jsonEncoder: Encoder[AssetBox] = ( box: AssetBox ) =>
    Map(
      "id" -> box.id.asJson,
      "type" -> box.typeOfBox.asJson,
      "proposition" -> box.proposition.asJson,
      "assetCode" -> box.assetCode.asJson,
      "value" -> box.value.toString.asJson,
      "issuer" -> box.issuer.asJson,
      "data" -> box.data.asJson,
      "nonce" -> box.nonce.toString.asJson
      ).asJson

  implicit val jsonDecoder: Decoder[AssetBox] = ( c: HCursor ) =>
    for {
      proposition <- c.downField("proposition").as[PublicKey25519Proposition]
      value <- c.downField("value").as[Long]
      nonce <- c.downField("issuer").as[Long]
      assetCode <- c.downField("assetCode").as[String]
      issuer <- c.downField("issuer").as[PublicKey25519Proposition]
      data <- c.downField("data").as[String]
    } yield {
      AssetBox(proposition, nonce, value, assetCode, issuer, data)
    }
}
