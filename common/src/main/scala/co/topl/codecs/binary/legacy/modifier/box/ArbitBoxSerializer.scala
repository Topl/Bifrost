package co.topl.codecs.binary.legacy.modifier.box

import co.topl.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.box.ArbitBox

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    EvidenceSerializer.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    SimpleValueSerializer.serialize(obj.value, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val evidence = EvidenceSerializer.parse(r)
    val nonce = r.getLong()
    val value = SimpleValueSerializer.parse(r)

    ArbitBox(evidence, nonce, value)
  }
}
