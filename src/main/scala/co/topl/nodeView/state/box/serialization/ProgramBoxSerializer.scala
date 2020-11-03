package co.topl.nodeView.state.box.serialization

import co.topl.attestation.proposition.{PublicKeyCurve25519Proposition, PublicKeyCurve25519PropositionSerializer}
import co.topl.nodeView.state.box.{ProgramBox, ProgramId}
import co.topl.utils.serialization.{Reader, Writer}

object ProgramBoxSerializer {

  def serialize(obj: ProgramBox, w: Writer): Unit = {
    PublicKeyCurve25519PropositionSerializer.serialize(obj.proposition, w)

    w.putLong(obj.nonce)

    // box identifier in the program box registry
    ProgramId.serialize(obj.value, w)
  }

  def parse(r: Reader): (PublicKeyCurve25519Proposition, Long, ProgramId) = {
    (PublicKeyCurve25519PropositionSerializer.parse(r), r.getLong(), ProgramId.parse(r))
  }
}
