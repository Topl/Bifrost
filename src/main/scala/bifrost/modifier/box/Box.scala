package bifrost.modifier.box

import bifrost.crypto.PrivateKey25519
import com.google.common.primitives.Ints
import io.circe.Json
import bifrost.serialization.Serializer
import bifrost.modifier.box.proposition.ProofOfKnowledgeProposition

import scala.util.Try

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class Box(proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                   val nonce: Long,
                   value: Any) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  override type M = Box

  override def serializer: Serializer[Box] = BoxSerializer

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

object BoxSerializer extends Serializer[Box] {

  override def toBytes(obj: Box): Array[Byte] = obj match {
    case p: PolyBox => PolyBoxSerializer.toBytes(p)
    case a: ArbitBox => ArbitBoxSerializer.toBytes(a)
    case as: AssetBox => AssetBoxSerializer.toBytes(as)
    case sb: StateBox => StateBoxSerializer.toBytes(sb)
    case cb: CodeBox => CodeBoxSerializer.toBytes(cb)
    case eb: ExecutionBox => ExecutionBoxSerializer.toBytes(eb)
    case _ => throw new Exception("Unanticipated Box type")
  }

  override def parseBytes(bytes: Array[Byte]): Try[Box] = {

    val typeLen = Ints.fromByteArray(bytes.take(Ints.BYTES))
    val typeStr: String = new String(bytes.slice(Ints.BYTES, Ints.BYTES + typeLen))

    typeStr match {
      case "ArbitBox" => ArbitBoxSerializer.parseBytes(bytes)
      case "AssetBox" => AssetBoxSerializer.parseBytes(bytes)
      case "PolyBox" => PolyBoxSerializer.parseBytes(bytes)
      case "StateBox" => StateBoxSerializer.parseBytes(bytes)
      case "CodeBox" => CodeBoxSerializer.parseBytes(bytes)
      case "ExecutionBox" => ExecutionBoxSerializer.parseBytes(bytes)
      case _ => throw new Exception("Unanticipated Box Type")
    }
  }
}
