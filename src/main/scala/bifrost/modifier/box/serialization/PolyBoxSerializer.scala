package bifrost.modifier.box.serialization

import bifrost.modifier.box.{PolyBox, TokenBox}
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object PolyBoxSerializer extends BifrostSerializer[PolyBox] {

  override def serialize(obj: PolyBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): PolyBox = {
    val tokenBox: TokenBox = TokenBoxSerializer.parse(r)
    PolyBox(tokenBox.proposition, tokenBox.nonce, tokenBox.value)
  }
}
