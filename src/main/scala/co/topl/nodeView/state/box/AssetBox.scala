package co.topl.nodeView.state.box

import co.topl.attestation.{Address, Evidence}
import co.topl.nodeView.state.box.Box.BoxType
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class AssetBox (override val evidence: Evidence,
                     override val nonce   : Box.Nonce,
                     override val value   : TokenBox.Value,
                     assetCode            : String,
                     issuer               : Address,
                     data                 : String
                    ) extends TokenBox(evidence, nonce, value, AssetBox.boxTypePrefix)

object AssetBox {
  val boxTypePrefix: BoxType = 3: Byte

  implicit val jsonEncoder: Encoder[AssetBox] = { box: AssetBox =>
    (Box.jsonEncode(box) ++ Map(
      "issuer" -> box.issuer.asJson,
      "assetCode" -> box.assetCode.asJson,
      "data" -> box.data.asJson)
      ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode[TokenBox.Value](c)
      issuer <- c.downField("issuer").as[Address]
      assetCode <- c.downField("assetCode").as[String]
      data <- c.downField("data").as[String]
    } yield {
      val (evidence, nonce, value) = b
      AssetBox(evidence, nonce, value, assetCode, issuer, data)
    }
}
