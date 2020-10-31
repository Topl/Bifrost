package co.topl.nodeView.state.box

import co.topl.attestation.proof.SignatureCurve25519
import co.topl.attestation.proposition.PublicKeyCurve25519Proposition
import co.topl.attestation.{ BoxUnlocker, Evidence, EvidenceProducer }
import io.circe.syntax.EncoderOps
import io.circe.{ DecodingFailure, HCursor, Json }

abstract class TokenBox ( override val evidence     : Evidence,
                          override val nonce        : Box.Nonce,
                          override val value        : TokenBox.Value,
                          override val boxTypePrefix: Box.BoxType
                        ) extends Box[TokenBox.Value](evidence, nonce, value, boxTypePrefix)


object TokenBox {
  type Value = Long

  /**
   * Generate a series of unlockers for a transactions that is used to validate the transaction
   *
   * @param from
   * @param signatures
   * @return
   */
  def generateUnlockers (from: Seq[(PublicKeyCurve25519Proposition, Box.Nonce)],
                         signatures: Map[PublicKeyCurve25519Proposition, SignatureCurve25519]
                        ): Traversable[BoxUnlocker[PublicKeyCurve25519Proposition]] = {
    from.map {
      case (prop, nonce) =>
        val boxId = PublicKeyNoncedBox.idFromBox(prop, nonce)
        val boxKey = signatures.getOrElse(prop, throw new Exception("Signature not provided"))
        new BoxUnlocker(boxId, boxKey)
    }
  }

  def generateUnlockers ( boxIds   : Seq[BoxId],
                          signature: SignatureCurve25519
                        ): Traversable[BoxUnlocker[PublicKeyCurve25519Proposition]] = {
    boxIds.map { id =>
      new BoxUnlocker(id, signature)
    }
  }
}