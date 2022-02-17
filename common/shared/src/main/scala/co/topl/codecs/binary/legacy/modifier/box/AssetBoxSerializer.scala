package co.topl.codecs.binary.legacy.modifier.box

import co.topl.codecs.binary.legacy.attestation.EvidenceSerializer
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.box.AssetBox

object AssetBoxSerializer extends BifrostSerializer[AssetBox] {

  override def serialize(obj: AssetBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    EvidenceSerializer.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    AssetValueSerializer.serialize(obj.value, w)
  }

  override def parse(r: Reader): AssetBox = {
    val evidence = EvidenceSerializer.parse(r)
    val nonce = r.getLong()
    val value = AssetValueSerializer.parse(r)

    AssetBox(evidence, nonce, value)
  }
}
