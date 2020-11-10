package co.topl.nodeView.state.box

import co.topl.attestation.{Address, Evidence}
import co.topl.attestation.proposition.PublicKeyPropositionCurve25519
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
      "data" -> box.data.asJson,
      "nonce" -> box.nonce.toString.asJson)
      ).asJson
  }

  implicit val jsonDecoder: Decoder[AssetBox] = ( c: HCursor ) =>
    for {
      b <- Box.jsonDecode[TokenBox.Value](c)
      assetCode <- c.downField("assetCode").as[String]
      issuer <- c.downField("issuer").as[Address]
      data <- c.downField("data").as[String]
    } yield {
      val (evidence, nonce, value) = b
      AssetBox(evidence, nonce, value, assetCode, issuer, data)
    }
}
