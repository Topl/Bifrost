package bifrost.modifier.box

import bifrost.crypto.PrivateKey25519
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition
import bifrost.utils.serialization.{BifrostSerializer, Reader, Writer}
import io.circe.Json

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class Box(proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                   val nonce: Long,
                   value: Any) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  override type M = Box

  override def serializer: BifrostSerializer[Box] = BoxSerializer

  // lazy val id: Array[Byte] = PublicKeyNoncedBox.idFromBox(proposition, nonce)

  lazy val publicKey: ProofOfKnowledgeProposition[PrivateKey25519] = proposition

  val typeOfBox: String

  val json: Json

  override def equals(obj: Any): Boolean = obj match {
    case acc: Box => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}

object BoxSerializer extends BifrostSerializer[Box] {

  override def serialize(obj: Box, w: Writer): Unit = {
    obj match {
      case p: PolyBox => PolyBoxSerializer.serialize(p, w)
      case a: ArbitBox => ArbitBoxSerializer.serialize(a, w)
      case as: AssetBox => AssetBoxSerializer.serialize(as, w)
      case sb: StateBox => StateBoxSerializer.serialize(sb, w)
      case cb: CodeBox => CodeBoxSerializer.serialize(cb, w)
      case eb: ExecutionBox => ExecutionBoxSerializer.serialize(eb, w)
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
