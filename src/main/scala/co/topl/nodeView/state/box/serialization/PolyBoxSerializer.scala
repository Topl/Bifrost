package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.box.PolyBox
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object PolyBoxSerializer extends BifrostSerializer[PolyBox] {

  override def serialize(obj: PolyBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): PolyBox = {
    val (proposition, nonce, value) = TokenBoxSerializer.parse(r)
    PolyBox(proposition, nonce, value)
  }
}
