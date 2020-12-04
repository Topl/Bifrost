package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.Box.BoxType
import co.topl.utils.HasName
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class AssetBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    AssetValue
) extends TokenBox(evidence, nonce, value, AssetBox.boxTypePrefix)

object AssetBox {
  val boxTypePrefix: BoxType = 3: Byte
  val boxTypeString: String = "AssetBox"

  implicit val name: HasName[AssetBox] = HasName.instance(() => boxTypeString)

  implicit val jsonEncoder: Encoder[AssetBox] = (box: AssetBox) => Box.jsonEncode[AssetValue, AssetBox](box).asJson

  implicit val jsonDecoder: Decoder[AssetBox] = (c: HCursor) =>
    Box.jsonDecode[AssetValue](c).map { case (evidence, nonce, value) =>
      AssetBox(evidence, nonce, value)
    }
}
