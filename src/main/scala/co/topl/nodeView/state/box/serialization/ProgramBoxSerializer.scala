package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.ProgramId
import co.topl.nodeView.state.box.ProgramBox
import co.topl.nodeView.state.box.proposition.{ PublicKey25519Proposition, PublicKey25519PropositionSerializer }
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ProgramBoxSerializer {

  def serialize(obj: ProgramBox, w: Writer): Unit = {
    PublicKey25519PropositionSerializer.serialize(obj.proposition, w)

    w.putLong(obj.nonce)

    // box identifier in the program box registry
    ProgramId.serialize(obj.value, w)
  }

  def parse(r: Reader): (PublicKey25519Proposition, Long, ProgramId) = {
    (PublicKey25519PropositionSerializer.parse(r), r.getLong(), ProgramId.parse(r))
  }
}
