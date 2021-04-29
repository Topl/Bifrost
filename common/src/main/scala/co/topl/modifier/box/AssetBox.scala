package co.topl.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.Box.BoxType
import co.topl.utils.{Identifiable, Identifier}
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor}

case class AssetBox(
  override val evidence: Evidence,
  override val nonce:    Box.Nonce,
  override val value:    AssetValue
) extends TokenBox(evidence, nonce, value)

object AssetBox {
  val typePrefix: BoxType = 3: Byte
  val typeString: String = "AssetBox"

  implicit val identifier: Identifiable[AssetBox] = Identifiable.instance { () =>
    Identifier(typeString, typePrefix)
  }

  implicit val jsonEncoder: Encoder[AssetBox] = (box: AssetBox) => Box.jsonEncode[AssetValue, AssetBox](box).asJson

  implicit val jsonDecoder: Decoder[AssetBox] = (c: HCursor) =>
    Box.jsonDecode[AssetValue](c).map { case (evidence, nonce, value) =>
      AssetBox(evidence, nonce, value)
    }
}
