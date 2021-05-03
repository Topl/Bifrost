package modifier

import attestation.Evidence
import utils.serialization.{GjalSerializer, Reader, Writer}

object BoxSerializer extends GjalSerializer[Box] {

  override def serialize(obj: Box, w: Writer): Unit = {
    w.put(Box.typePrefix(obj))
    Evidence.serialize(obj.evidence, w)
    w.putLong(obj.nonce)
    obj.value match {
      case token: SimpleValue => SimpleValue.serialize(token, w)
      case asset: AssetValue  => AssetValue.serialize(asset, w)
    }
  }

  override def parse(r: Reader): Box = {
    val evidence = Evidence.parse(r)
    val nonce = r.getLong()
    r.getByte() match {
      case 1 =>
        val value = SimpleValue.parse(r)
        Box(evidence, nonce, "ArbitBox", value)
      case 2 =>
        val value = SimpleValue.parse(r)
        Box(evidence, nonce, "PolyBox", value)
      case 3 =>
        val value = AssetValue.parse(r)
        Box(evidence, nonce, "AssetBox", value)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}
