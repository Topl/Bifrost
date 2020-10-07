package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.box.TokenBox
import co.topl.nodeView.state.box.proposition.PublicKey25519PropositionSerializer
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object TokenBoxSerializer extends BifrostSerializer[TokenBox] {

  override def serialize(obj: TokenBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  override def parse(r: Reader): TokenBox = {
    new TokenBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      r.getULong()
    )
  }
}
