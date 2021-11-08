package co.topl.codecs.binary.legacy.modifier

import co.topl.modifier.ModifierId
import co.topl.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object ModifierIdSerializer extends BifrostSerializer[ModifierId] {

  def serialize(obj: ModifierId, w: Writer): Unit =
    /* value: Array[Byte] */
    w.putBytes(obj.value)

  def parse(r: Reader): ModifierId = {
    val value: Array[Byte] = r.getBytes(ModifierId.size)
    new ModifierId(value)
  }

}
