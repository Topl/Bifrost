package bifrost.nodeView.box.serialization

import bifrost.nodeView.box.{ ArbitBox, TokenBox }
import bifrost.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val tokenBox: TokenBox = TokenBoxSerializer.parse(r)
    ArbitBox(tokenBox.proposition, tokenBox.nonce, tokenBox.value)
  }
}
