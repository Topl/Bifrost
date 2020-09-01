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

  //  TODO: Jing - remove
  //
  //  override def toBytes(obj: Box): Array[Byte] = obj match {
  //    case p: PolyBox => PolyBoxSerializer.toBytes(p)
  //    case a: ArbitBox => ArbitBoxSerializer.toBytes(a)
  //    case as: AssetBox => AssetBoxSerializer.toBytes(as)
  //    case sb: StateBox => StateBoxSerializer.toBytes(sb)
  //    case cb: CodeBox => CodeBoxSerializer.toBytes(cb)
  //    case eb: ExecutionBox => ExecutionBoxSerializer.toBytes(eb)
  //    case _ => throw new Exception("Unanticipated Box type")
  //  }
  //
  //  override def parseBytes(bytes: Array[Byte]): Try[Box] = {
  //
  //    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
  //    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))
  //
  //    typeStr match {
  //      case "ArbitBox" => ArbitBoxSerializer.parseBytes(bytes)
  //      case "AssetBox" => AssetBoxSerializer.parseBytes(bytes)
  //      case "PolyBox" => PolyBoxSerializer.parseBytes(bytes)
  //      case "StateBox" => StateBoxSerializer.parseBytes(bytes)
  //      case "CodeBox" => CodeBoxSerializer.parseBytes(bytes)
  //      case "ExecutionBox" => ExecutionBoxSerializer.parseBytes(bytes)
  //      case _ => throw new Exception("Unanticipated Box Type")
  //    }
  //  }
}