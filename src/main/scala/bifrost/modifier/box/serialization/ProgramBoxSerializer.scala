package bifrost.modifier.box.serialization

import bifrost.modifier.box.ProgramBox
import bifrost.modifier.box.proposition.PublicKey25519PropositionSerializer
import bifrost.state.ProgramId
import bifrost.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ProgramBoxSerializer extends BifrostSerializer[ProgramBox] {

  override def serialize(obj: ProgramBox, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    w.putLong(obj.nonce)

    // box identifier in the program box registry
    ProgramId.serialize(obj.value, w)
  }

  override def parse(r: Reader): ProgramBox = {
    new ProgramBox(
      PublicKey25519PropositionSerializer.parse(r),
      r.getLong(),
      ProgramId.parse(r),
    )
  }
}
