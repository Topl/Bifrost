package bifrost.modifier.box.serialization

import bifrost.modifier.box.NoncedBox
import bifrost.modifier.box.proposition.PublicKey25519PropositionSerializer
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object NoncedBoxSerializer extends BifrostSerializer[NoncedBox] {

  override def serialize(obj: NoncedBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    w.putULong(obj.value)
  }

  override def parse(r: Reader): NoncedBox = {
    new NoncedBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      r.getULong()
    )
  }
}
