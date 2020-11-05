package co.topl.nodeView.state.box

import co.topl.attestation.Evidence

abstract class TokenBox ( override val evidence     : Evidence,
                          override val nonce        : Box.Nonce,
                          override val value        : TokenBox.Value,
                          override val boxTypePrefix: Box.BoxType
                        ) extends Box[TokenBox.Value](evidence, nonce, value, boxTypePrefix)


object TokenBox {
  type Value = Long
}