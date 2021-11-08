package co.topl.typeclasses

import co.topl.crypto.signing.{Curve25519, Ed25519, Ed25519VRF}
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.language.implicitConversions
import scala.util.Try

trait ProofVerifier[Proof, Proposition] {

  /**
   * Does the given `proof` satisfy the given `proposition` using the given `data`?
   */
  def verifyWith[Data: Signable](proof: Proof, proposition: Proposition, data: Data): Boolean
}

// todo: James - I really want to make the typeclasses just act as controllers to the functionality implemented
// elsewhere, it should be the responsibility of the implmentation to present interfaces (i.e. type usage) that the
// typeclasses will be expecting. This way we have a common set of models that we can assume all modules will
// use for their API
object ProofVerifier {

  trait Ops[Proof, Proposition] {

    def proof: Proof
    def typeclassInstance: ProofVerifier[Proof, Proposition]

    def satisfies[Data: Signable](proposition: Proposition, data: Data): Boolean =
      typeclassInstance.verifyWith(proof, proposition, data)
  }

  trait Implicits {

    implicit def asVerifierOps[Proof, Proposition](
      p:                 Proof
    )(implicit verifier: ProofVerifier[Proof, Proposition]): Ops[Proof, Proposition] =
      new Ops[Proof, Proposition] {
        override def proof: Proof = p

        override def typeclassInstance: ProofVerifier[Proof, Proposition] = verifier
      }
  }

  object ops extends Implicits

  trait Instances {

    implicit val publicKeyCurve25519: ProofVerifier[Proofs.Signature.Curve25519, Propositions.Knowledge.Curve25519] =
      new ProofVerifier[Proofs.Signature.Curve25519, Propositions.Knowledge.Curve25519] {
        private val curve25519 = new Curve25519()

        override def verifyWith[Data: Signable](
          proof:       Proofs.Signature.Curve25519,
          proposition: Propositions.Knowledge.Curve25519,
          data:        Data
        ): Boolean = curve25519.verify(
          proof,
          data.signableBytes,
          proposition.key
        )
      }

    implicit val publicKeyEd25519: ProofVerifier[Proofs.Signature.Ed25519, Propositions.Knowledge.Ed25519] =
      new ProofVerifier[Proofs.Signature.Ed25519, Propositions.Knowledge.Ed25519] {
        private val ed25519 = new Ed25519()

        override def verifyWith[Data: Signable](
          proof:       Proofs.Signature.Ed25519,
          proposition: Propositions.Knowledge.Ed25519,
          data:        Data
        ): Boolean = ed25519.verify(
          proof,
          data.signableBytes,
          proposition.key
        )
      }

    // todo: move this logic to an implementation
    implicit val thresholdCurve25519
      : ProofVerifier[Proofs.Threshold.Curve25519, Propositions.Knowledge.Threshold.Curve25519] =
      new ProofVerifier[Proofs.Threshold.Curve25519, Propositions.Knowledge.Threshold.Curve25519] {
        private val curve25519 = new Curve25519()

        override def verifyWith[Data: Signable](
          proof:       Proofs.Threshold.Curve25519,
          proposition: Propositions.Knowledge.Threshold.Curve25519,
          data:        Data
        ): Boolean = {
          val dataBytes = data.signableBytes.toArray
          proposition.propositions.size >= proposition.threshold && {
            val (validSignatureCount, _) =
              proof.signatures
                .foldLeft((0, proposition.propositions)) { case ((acc, unusedProps), sig) =>
                  if (acc < proposition.threshold) {
                    unusedProps
                      .find(prop =>
                        unusedProps(prop) && curve25519.verify(
                          sig,
                          Bytes(dataBytes),
                          prop
                        )
                      ) match {
                      case Some(prop) =>
                        (acc + 1, unusedProps.diff(Set(prop)))
                      case None =>
                        (acc, unusedProps)
                    }
                  } else (acc, unusedProps)
                }
            validSignatureCount >= proposition.threshold
          }
        }
      }

    // todo: move this logic to an implemntation
    implicit val thresholdEd25519: ProofVerifier[Proofs.Threshold.Ed25519, Propositions.Knowledge.Threshold.Ed25519] =
      new ProofVerifier[Proofs.Threshold.Ed25519, Propositions.Knowledge.Threshold.Ed25519] {

        private val ed25519 = new Ed25519()

        override def verifyWith[Data: Signable](
          proof:       Proofs.Threshold.Ed25519,
          proposition: Propositions.Knowledge.Threshold.Ed25519,
          data:        Data
        ): Boolean =
          proposition.propositions.size >= proposition.threshold && {
            val (validSignatureCount, _) =
              proof.signatures
                .foldLeft((0, proposition.propositions)) { case ((acc, unusedProps), sig) =>
                  if (acc < proposition.threshold) {
                    unusedProps
                      .find(prop =>
                        unusedProps(prop) && ed25519.verify(
                          sig,
                          data.signableBytes,
                          prop
                        )
                      ) match {
                      case Some(prop) =>
                        (acc + 1, unusedProps.diff(Set(prop)))
                      case None =>
                        (acc, unusedProps)
                    }
                  } else (acc, unusedProps)
                }
            validSignatureCount >= proposition.threshold
          }
      }

    implicit val signatureVrfEd25519: ProofVerifier[Proofs.Signature.VrfEd25519, Propositions.VerificationKeyVRF] =
      new ProofVerifier[Proofs.Signature.VrfEd25519, Propositions.VerificationKeyVRF] {

        override def verifyWith[Data: Signable](
          proof:       Proofs.Signature.VrfEd25519,
          proposition: Propositions.VerificationKeyVRF,
          data:        Data
        ): Boolean =
          Try(
            Ed25519VRF.instance.verify(
              proof,
              data.signableBytes,
              proposition.key
            )
          ).getOrElse(false)
      }
  }

  object Instances extends Instances
}
