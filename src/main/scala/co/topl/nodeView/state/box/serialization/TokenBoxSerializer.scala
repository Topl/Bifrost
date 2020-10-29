package co.topl.nodeView.state.box.serialization

import co.topl.attestation.proposition.{PublicKeyCurve25519Proposition, PublicKeyCurve25519PropositionSerializer}
import co.topl.nodeView.state.box.TokenBox
import co.topl.utils.serialization.{Reader, Writer}

object TokenBoxSerializer {

  def serialize(obj: TokenBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    PublicKeyCurve25519PropositionSerializer.serialize(obj.proposition, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  def parse(r: Reader): (PublicKeyCurve25519Proposition, Long, Long) = {
    (PublicKeyCurve25519PropositionSerializer.parse(r), r.getLong(), r.getULong())
  }
}
