package co.topl.codecs.binary.legacy.modifier.box

import co.topl.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.box.PolyBox

object PolyBoxSerializer extends BifrostSerializer[PolyBox] {

  override def serialize(obj: PolyBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    EvidenceSerializer.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    SimpleValueSerializer.serialize(obj.value, w)
  }

  override def parse(r: Reader): PolyBox = {
    val evidence = EvidenceSerializer.parse(r)
    val nonce = r.getLong()
    val value = SimpleValueSerializer.parse(r)

    PolyBox(evidence, nonce, value)
  }
}
