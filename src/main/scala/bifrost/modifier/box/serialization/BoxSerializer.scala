package bifrost.modifier.box.serialization

import bifrost.modifier.box._
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}

object BoxSerializer extends BifrostSerializer[Box] {

  override def serialize(obj: Box, w: Writer): Unit = {
    obj match {
      case p: PolyBox =>
        w.putByteString("PolyBox")
        PolyBoxSerializer.serialize(p, w)
      case a: ArbitBox =>
        w.putByteString("ArbitBox")
        ArbitBoxSerializer.serialize(a, w)
      case as: AssetBox =>
        w.putByteString("AssetBox")
        AssetBoxSerializer.serialize(as, w)
      case sb: StateBox =>
        w.putByteString("StateBox")
        StateBoxSerializer.serialize(sb, w)
      case cb: CodeBox =>
        w.putByteString("CodeBox")
        CodeBoxSerializer.serialize(cb, w)
      case eb: ExecutionBox =>
        w.putByteString("ExecutionBox")
        ExecutionBoxSerializer.serialize(eb, w)
      case _ => throw new Exception("Unanticipated Box type")
    }
  }

  override def parse(r: Reader): Box = {
    r.getByteString() match {
      case "ArbitBox" => ArbitBoxSerializer.parse(r)
      case "AssetBox" => AssetBoxSerializer.parse(r)
      case "PolyBox" => PolyBoxSerializer.parse(r)
      case "StateBox" => StateBoxSerializer.parse(r)
      case "CodeBox" => CodeBoxSerializer.parse(r)
      case "ExecutionBox" => ExecutionBoxSerializer.parse(r)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}
