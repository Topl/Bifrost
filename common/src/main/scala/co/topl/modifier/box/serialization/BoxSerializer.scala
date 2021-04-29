package co.topl.modifier.box.serialization

import co.topl.modifier.box._
import co.topl.utils.serialization.{BifrostSerializer, Reader, Writer}

object BoxSerializer extends BifrostSerializer[Box[_]] {

  override def serialize(obj: Box[_], w: Writer): Unit =
    obj match {
      case obj: PolyBox =>
        w.put(PolyBox.typePrefix)
        PolyBoxSerializer.serialize(obj, w)

      case obj: ArbitBox =>
        w.put(ArbitBox.typePrefix)
        ArbitBoxSerializer.serialize(obj, w)

      case obj: AssetBox =>
        w.put(AssetBox.typePrefix)
        AssetBoxSerializer.serialize(obj, w)

      case obj: StateBox =>
        w.put(StateBox.typePrefix)
        StateBoxSerializer.serialize(obj, w)

      case obj: CodeBox =>
        w.put(CodeBox.typePrefix)
        CodeBoxSerializer.serialize(obj, w)

      case obj: ExecutionBox =>
        w.put(ExecutionBox.typePrefix)
        ExecutionBoxSerializer.serialize(obj, w)

      case _ => throw new Exception("Unanticipated Box type")
    }

  override def parse(r: Reader): Box[_] =
    r.getByte() match {
      case ArbitBox.typePrefix     => ArbitBoxSerializer.parse(r)
      case AssetBox.typePrefix     => AssetBoxSerializer.parse(r)
      case PolyBox.typePrefix      => PolyBoxSerializer.parse(r)
      case StateBox.typePrefix     => StateBoxSerializer.parse(r)
      case CodeBox.typePrefix      => CodeBoxSerializer.parse(r)
      case ExecutionBox.typePrefix => ExecutionBoxSerializer.parse(r)
      case _                       => throw new Exception("Unanticipated Box Type")
    }
}
