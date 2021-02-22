package co.topl.nodeView.state.box.serialization

import co.topl.attestation.Evidence
import co.topl.nodeView.state.box.{AssetBox, AssetValue}
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object AssetBoxSerializer extends BifrostSerializer[AssetBox] {
  override def serialize(obj: AssetBox, w: Writer): Unit = {
    /* proposition: PublicKey25519Proposition */
    Evidence.serialize(obj.evidence, w)

    /* nonce: Long */
    w.putLong(obj.nonce)

    /* value: Long */
    AssetValue.serialize(obj.value, w)
  }

  override def parse(r: Reader): AssetBox = {
    val evidence = Evidence.parse(r)
    val nonce = r.getLong()
    val value = AssetValue.parse(r)

    AssetBox(evidence, nonce, value)
  }
}