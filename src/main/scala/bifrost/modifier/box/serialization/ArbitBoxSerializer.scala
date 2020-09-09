package bifrost.modifier.box.serialization

import bifrost.modifier.box.{ArbitBox, TokenBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val noncedBox: TokenBox = TokenBoxSerializer.parse(r)
    ArbitBox(noncedBox.proposition, noncedBox.nonce, noncedBox.value)
  }
}
