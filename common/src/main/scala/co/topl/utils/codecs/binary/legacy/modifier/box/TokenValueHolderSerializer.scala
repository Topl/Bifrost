package co.topl.utils.codecs.binary.legacy.modifier.box

import co.topl.modifier.box.{AssetValue, SimpleValue, TokenValueHolder}
import co.topl.utils.codecs.binary.legacy.{BifrostSerializer, Reader, Writer}

object TokenValueHolderSerializer extends BifrostSerializer[TokenValueHolder] {

  override def serialize(obj: TokenValueHolder, w: Writer): Unit =
    obj match {
      case obj: SimpleValue =>
        w.put(SimpleValue.valueTypePrefix)
        SimpleValueSerializer.serialize(obj, w)

      case obj: AssetValue =>
        w.put(AssetValue.valueTypePrefix)
        AssetValueSerializer.serialize(obj, w)

      case _ => throw new Exception("Unanticipated TokenValueType type")
    }

  override def parse(r: Reader): TokenValueHolder =
    r.getByte() match {
      case SimpleValue.valueTypePrefix => SimpleValueSerializer.parse(r)
      case AssetValue.valueTypePrefix  => AssetValueSerializer.parse(r)
      case _                           => throw new Exception("Unanticipated Box Type")
    }
}
