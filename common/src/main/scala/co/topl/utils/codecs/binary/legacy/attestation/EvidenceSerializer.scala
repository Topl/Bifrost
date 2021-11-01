package co.topl.utils.codecs.binary.legacy.attestation

import co.topl.attestation.Evidence
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object EvidenceSerializer extends BifrostSerializer[Evidence] {

  override def serialize(obj: Evidence, w: Writer): Unit =
    w.putBytes(obj.evBytes)

  override def parse(r: Reader): Evidence = {
    val evBytes = r.getBytes(Evidence.size)
    new Evidence(evBytes)
  }
}
