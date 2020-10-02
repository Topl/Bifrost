package bifrost.nodeView.box.serialization

import bifrost.nodeView.box.TokenBox
import bifrost.nodeView.box.proposition.PublicKey25519PropositionSerializer
import bifrost.utils.serialization.{ BifrostSerializer, Reader, Writer }

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
