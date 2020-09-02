package bifrost.modifier.box.serialization

import bifrost.modifier.box.{NoncedBox, PolyBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object PolyBoxSerializer extends BifrostSerializer[PolyBox] {

  override def serialize(obj: PolyBox, w: Writer): Unit = {
    NoncedBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): PolyBox = {
    val noncedBox: NoncedBox = NoncedBoxSerializer.parse(r)
    PolyBox(noncedBox.proposition, noncedBox.nonce, noncedBox.value)
  }
}
