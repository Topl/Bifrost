package bifrost.nodeView.box

import bifrost.crypto.PrivateKey25519
import bifrost.nodeView.box.proposition.ProofOfKnowledgeProposition
import bifrost.nodeView.box.serialization.BoxSerializer
import bifrost.utils.serialization.BifrostSerializer
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
