package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class AssetBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    AssetValue
) extends TokenBox(evidence, nonce, value, AssetBox.boxTypePrefix)

object AssetBox {
  val boxTypePrefix: BoxType = 3: Byte

  implicit val jsonEncoder: Encoder[AssetBox] = (box: AssetBox) => Box.jsonEncode(box).asJson

  implicit val jsonDecoder: Decoder[AssetBox] = (c: HCursor) =>
    Box.jsonDecode[AssetValue](c).map { case (evidence, nonce, value) =>
      AssetBox(evidence, nonce, value)
    }
}
