package bifrost.modifier.box.serialization

import java.util.UUID

import bifrost.modifier.box.ProgramBox
import bifrost.modifier.box.proposition.PublicKey25519PropositionSerializer
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object ProgramBoxSerializer extends BifrostSerializer[ProgramBox] {

  override def serialize(obj: ProgramBox, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)
    w.putLong(obj.nonce)

    // The SignificantBits could be negative longs
    w.putLong(obj.value.getMostSignificantBits)
    w.putLong(obj.value.getLeastSignificantBits)
  }

  override def parse(r: Reader): ProgramBox = {
    new ProgramBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      /* A UUID represents a 128-bit value, the two longs are the most and least significant 64 bits of this UUID */
      new UUID(r.getLong(), r.getLong())
    )
  }
}
