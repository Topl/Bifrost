package co.topl.attestation.proof

import co.topl.attestation.proposition.{ KnowledgeProposition, Proposition }
import co.topl.attestation.secrets.Secret
import co.topl.utils.serialization.BytesSerializable

/**
  * The most general abstraction of fact a prover can provide a non-interactive proof
  * to open a box or to modify an account
  *
  * A proof is non-interactive and thus serializable
  */

trait Proof[+P <: Proposition] extends BytesSerializable {
  def isValid(proposition: Proposition, message: Array[Byte]): Boolean
}

trait ProofOfKnowledge[S <: Secret, P <: KnowledgeProposition[S]] extends Proof[P]
