package co.topl.modifier.box.serialization

import co.topl.attestation.Evidence
import co.topl.modifier.box.{ProgramBox, ProgramId}
import co.topl.utils.serialization.{Reader, Writer}

object ProgramBoxSerializer {

  def serialize(obj: ProgramBox, w: Writer): Unit = {
    Evidence.serialize(obj.evidence, w)

    w.putLong(obj.nonce)

    // box identifier in the program box registry
    ProgramId.serialize(obj.value, w)
  }

  def parse(r: Reader): (Evidence, Long, ProgramId) = {
    (Evidence.parse(r), r.getLong(), ProgramId.parse(r))
  }
}
