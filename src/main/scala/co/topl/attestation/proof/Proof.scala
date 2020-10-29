package co.topl.attestation.proof

import co.topl.attestation.{KnowledgeProposition, Proposition, Secret}
import co.topl.utils.serialization.{BifrostSerializer, BytesSerializable}
import scorex.util.encode.Base58

/**
  * The most general abstraction of fact a prover can provide a non-interactive proof
  * to open a box or to modify an account
  *
  * A proof is non-interactive and thus serializable
  */
trait Proof[P <: Proposition] extends BytesSerializable {
  override type M = Proof[_ <: Proposition]
  override def serializer: BifrostSerializer[Proof[_ <: Proposition]] = ProofSerializer

  def isValid(proposition: P, message: Array[Byte]): Boolean
}

trait ProofOfKnowledge[S <: Secret, P <: KnowledgeProposition[S]] extends Proof[P]
