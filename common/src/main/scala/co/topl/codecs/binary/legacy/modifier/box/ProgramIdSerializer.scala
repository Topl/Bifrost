package co.topl.codecs.binary.legacy.modifier.box

import co.topl.modifier.box.ProgramId
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object ProgramIdSerializer extends BifrostSerializer[ProgramId] {

  override def serialize(obj: ProgramId, w: Writer): Unit =
    w.putBytes(obj.hashBytes)

  override def parse(r: Reader): ProgramId =
    ProgramId(r.getBytes(ProgramId.size))
}
