package co.topl.modifier.box.serialization

import co.topl.attestation.Evidence
import co.topl.modifier.box.{ArbitBox, SimpleValue}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    Evidence.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    SimpleValue.serialize(obj.value, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val evidence = Evidence.parse(r)
    val nonce = r.getLong()
    val value = SimpleValue.parse(r)

    ArbitBox(evidence, nonce, value)
  }
}
