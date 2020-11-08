package co.topl.nodeView.state.box

import co.topl.attestation.Evidence
import io.circe.Encoder

abstract class TokenBox ( override val evidence     : Evidence,
                          override val nonce        : Box.Nonce,
                          override val value        : TokenBox.Value,
                          override val boxTypePrefix: Box.BoxType
                        ) extends Box[TokenBox.Value](evidence, nonce, value, boxTypePrefix)


object TokenBox {
  type Value = Long

  implicit def jsonEncoder: Encoder[TokenBox] = {
    case box: ArbitBox => ArbitBox.jsonEncoder(box)
    case box: PolyBox  => PolyBox.jsonEncoder(box)
    case box: AssetBox => AssetBox.jsonEncoder(box)
  }
}