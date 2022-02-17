package co.topl.codecs.binary.legacy.modifier.box

import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}
import co.topl.modifier.box.SimpleValue

object SimpleValueSerializer extends BifrostSerializer[SimpleValue] {

  override def serialize(obj: SimpleValue, w: Writer): Unit =
    w.putInt128(obj.quantity)

  override def parse(r: Reader): SimpleValue =
    SimpleValue(r.getInt128())
}
