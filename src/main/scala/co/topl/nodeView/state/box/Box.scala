package co.topl.nodeView.state.box

import co.topl.crypto.PrivateKey25519
import co.topl.nodeView.state.box.proposition.ProofOfKnowledgeProposition
import co.topl.nodeView.state.box.serialization.BoxSerializer
import co.topl.utils.serialization.BifrostSerializer
import io.circe.Json

/**
  * Created by Matthew on 4/11/2017.
  */
abstract class Box(proposition: ProofOfKnowledgeProposition[PrivateKey25519],
                   val nonce: Long,
                   value: Any) extends GenericBox[ProofOfKnowledgeProposition[PrivateKey25519], Any] {

  override type M = Box

  val typeOfBox: String

  lazy val publicKey: ProofOfKnowledgeProposition[PrivateKey25519] = proposition

  override def serializer: BifrostSerializer[Box] = BoxSerializer

  override def equals(obj: Any): Boolean = obj match {
    case acc: Box => (acc.id sameElements this.id) && acc.value == this.value
    case _ => false
  }

  override def hashCode(): Int = proposition.hashCode()
}
