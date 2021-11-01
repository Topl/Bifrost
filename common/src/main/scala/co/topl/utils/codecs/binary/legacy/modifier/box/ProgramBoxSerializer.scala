package co.topl.utils.codecs.binary.legacy.modifier.box

import co.topl.attestation.Evidence
import co.topl.modifier.box.{ProgramBox, ProgramId}
import co.topl.utils.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.utils.codecs.binary.legacy.{Reader, Writer}

object ProgramBoxSerializer {

  def serialize(obj: ProgramBox, w: Writer): Unit = {
    EvidenceSerializer.serialize(obj.evidence, w)

    w.putLong(obj.nonce)

    // box identifier in the program box registry
    ProgramIdSerializer.serialize(obj.value, w)
  }

  def parse(r: Reader): (Evidence, Long, ProgramId) =
    (EvidenceSerializer.parse(r), r.getLong(), ProgramIdSerializer.parse(r))
}
