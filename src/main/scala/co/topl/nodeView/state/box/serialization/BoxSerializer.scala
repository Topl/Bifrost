package co.topl.nodeView.state.box.serialization

import co.topl.nodeView.state.box._
import co.topl.utils.serialization.{ BifrostSerializer, Reader, Writer }

object BoxSerializer extends BifrostSerializer[Box[_]] {

  override def serialize ( obj: Box[_], w: Writer ): Unit = {
    obj match {
      case obj: PolyBox =>
        w.put(PolyBox.boxTypePrefix)
        PolyBoxSerializer.serialize(obj, w)

      case obj: ArbitBox =>
        w.put(ArbitBox.boxTypePrefix)
        ArbitBoxSerializer.serialize(obj, w)

      case obj: AssetBox =>
        w.put(AssetBox.boxTypePrefix)
        AssetBoxSerializer.serialize(obj, w)

      case obj: StateBox =>
        w.put(StateBox.boxTypePrefix)
        StateBoxSerializer.serialize(obj, w)

      case obj: CodeBox =>
        w.put(CodeBox.boxTypePrefix)
        CodeBoxSerializer.serialize(obj, w)

      case obj: ExecutionBox =>
        w.put(ExecutionBox.boxTypePrefix)
        ExecutionBoxSerializer.serialize(obj, w)

      case _                 => throw new Exception("Unanticipated Box type")
    }
  }

  override def parse ( r: Reader ): Box[_] = {
    r.getByte() match {
      case ArbitBox.boxTypePrefix     => ArbitBoxSerializer.parse(r)
      case AssetBox.boxTypePrefix     => AssetBoxSerializer.parse(r)
      case PolyBox.boxTypePrefix      => PolyBoxSerializer.parse(r)
      case StateBox.boxTypePrefix     => StateBoxSerializer.parse(r)
      case CodeBox.boxTypePrefix      => CodeBoxSerializer.parse(r)
      case ExecutionBox.boxTypePrefix => ExecutionBoxSerializer.parse(r)
      case _              => throw new Exception("Unanticipated Box Type")
    }
  }
}
