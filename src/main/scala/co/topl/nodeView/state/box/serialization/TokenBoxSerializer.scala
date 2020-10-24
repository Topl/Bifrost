package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.box.TokenBox
import co.topl.crypto.proposition.PublicKey25519Proposition
import co.topl.crypto.proposition.serialization.PublicKey25519PropositionSerializer
import co.topl.utils.serialization.{ Reader, Writer }

object TokenBoxSerializer {

  def serialize(obj: TokenBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  def parse(r: Reader): (PublicKey25519Proposition, Long, Long) = {
    (PublicKey25519PropositionSerializer.parse(r), r.getLong(), r.getULong())
  }
}
