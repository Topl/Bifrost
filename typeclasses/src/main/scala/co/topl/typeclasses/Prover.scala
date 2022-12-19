package co.topl.typeclasses

import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.signing.Ed25519
import co.topl.models._
import co.topl.models.utility.Sized
import co.topl.proto.models.{PropositionKnowledgeEd25519, VerificationKeyEd25519}
import com.google.protobuf.ByteString
import co.topl.models.utility.HasLength.instances.bytesLength

@simulacrum.typeclass
trait Prover[ProofInput] {

  /**
   * Creates a Proof using this Prover's input type
   * @param t a value which can construct a Proof, usually a SecretKey
   * @return a Proof
   */
  @simulacrum.op("asProof")
  def proveWith(t: ProofInput): Proof
}

object Prover {

  trait Instances {

    // TODO in PR BN-714 v3, we should stop depending typeclasses on models, and move to proto Models
    implicit def ed25519Proves(implicit
      ed: Ed25519
    ): Prover[(SecretKeys.Ed25519, Transaction.Unproven)] =
      (t: (SecretKeys.Ed25519, Transaction.Unproven)) => {
        val privateKey: PropositionKnowledgeEd25519 =
          PropositionKnowledgeEd25519.of(Some(
            VerificationKeyEd25519.of(
              ByteString.copyFrom(t._1.bytes.data.toArray)
            )
          ))
        val proofKnowledgeEd2551 = ed.sign(privateKey, t._2.signableBytes)
        Proofs.Knowledge.Ed25519(Sized.strictUnsafe(Bytes(proofKnowledgeEd2551.value.toByteArray)))
      }
  }

  object instances extends Instances
}
