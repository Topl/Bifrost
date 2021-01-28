package co.topl.modifier.box.serialization

import co.topl.attestation.Evidence
import co.topl.modifier.box.{PolyBox, SimpleValue}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object PolyBoxSerializer extends BifrostSerializer[PolyBox] {

  override def serialize(obj: PolyBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    Evidence.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    SimpleValue.serialize(obj.value, w)
  }

  override def parse(r: Reader): PolyBox = {
    val evidence = Evidence.parse(r)
    val nonce = r.getLong()
    val value = SimpleValue.parse(r)

    PolyBox(evidence, nonce, value)
  }
}
