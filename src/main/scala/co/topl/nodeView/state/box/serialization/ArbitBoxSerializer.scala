package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.box.ArbitBox
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object ArbitBoxSerializer extends BifrostSerializer[ArbitBox] {

  override def serialize(obj: ArbitBox, w: Writer): Unit = {
    TokenBoxSerializer.serialize(obj, w)
  }

  override def parse(r: Reader): ArbitBox = {
    val (evidence, nonce, value) = TokenBoxSerializer.parse(r)
    ArbitBox(evidence, nonce, value)
  }
}
