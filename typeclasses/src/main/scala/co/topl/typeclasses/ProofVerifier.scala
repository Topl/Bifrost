package co.topl.typeclasses

import cats._
import cats.implicits._
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.language.implicitConversions

trait ProofVerifier[F[_], Prop <: Proposition, Prf <: Proof] {

  /**
   * Does the given `proof` satisfy the given `proposition` using the given `data`?
   */
  def verifyWith(proposition: Prop, proof: Prf, context: VerificationContext[F]): F[Boolean]
}

object ProofVerifier {

  trait Ops[F[_], Prop <: Proposition, Prf <: Proof] {

    def proof: Prf
    def typeclassInstance: ProofVerifier[F, Prop, Prf]

    def satisfies(proposition: Prop, context: VerificationContext[F]): F[Boolean] =
      typeclassInstance.verifyWith(proposition, proof, context)
  }

  trait Implicits {

    implicit class ProofOps(proof: Proof) {

      def satisfies[F[_]](proposition: Proposition, context: VerificationContext[F])(implicit
        ev:                            ProofVerifier[F, Proposition, Proof]
      ): F[Boolean] =
        ev.verifyWith(proposition, proof, context)

    }

    implicit def asVerifierOps[F[_], Prop <: Proposition, Prf <: Proof](
      p:                 Prf
    )(implicit verifier: ProofVerifier[F, Prop, Prf]): Ops[F, Prop, Prf] =
      new Ops[F, Prop, Prf] {
        override def proof: Prf = p

        override def typeclassInstance: ProofVerifier[F, Prop, Prf] = verifier
      }
  }

  object ops extends Implicits

  trait Instances {

    implicit def publicKeyCurve25519[F[_]: Applicative]
      : ProofVerifier[F, Propositions.Knowledge.Curve25519, Proofs.Knowledge.Curve25519] =
      new ProofVerifier[F, Propositions.Knowledge.Curve25519, Proofs.Knowledge.Curve25519] {
        private val curve25519 = new Curve25519()

        override def verifyWith(
          proposition: Propositions.Knowledge.Curve25519,
          proof:       Proofs.Knowledge.Curve25519,
          context:     VerificationContext[F]
        ): F[Boolean] = curve25519
          .verify(
            proof,
            context.currentTransaction.signableBytes,
            proposition.key
          )
          .pure[F]
      }

    implicit def publicKeyEd25519[F[_]: Applicative](implicit
      ed25519: Ed25519
    ): ProofVerifier[F, Propositions.Knowledge.Ed25519, Proofs.Knowledge.Ed25519] =
      (proposition: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519, context: VerificationContext[F]) =>
        ed25519
          .verify(
            proof,
            context.currentTransaction.signableBytes,
            proposition.key
          )
          .pure[F]

    implicit def publicKeyExtendedEd25519[F[_]: Applicative](implicit
      extendedEd25519: ExtendedEd25519
    ): ProofVerifier[F, Propositions.Knowledge.ExtendedEd25519, Proofs.Knowledge.Ed25519] =
      (
        proposition: Propositions.Knowledge.ExtendedEd25519,
        proof:       Proofs.Knowledge.Ed25519,
        context:     VerificationContext[F]
      ) =>
        extendedEd25519
          .verify(
            proof,
            context.currentTransaction.signableBytes,
            proposition.key
          )
          .pure[F]

    implicit def heightLockVerifier[F[_]: Functor]
      : ProofVerifier[F, Propositions.Contextual.HeightLock, Proofs.Contextual.HeightLock] =
      (
        proposition: Propositions.Contextual.HeightLock,
        proof:       Proofs.Contextual.HeightLock,
        context:     VerificationContext[F]
      ) => context.heightOfBlock(proof.blockId).map(_.exists(_ >= proposition.height))

    implicit def thresholdVerifier[F[_]: Monad](implicit
      proofVerifier: ProofVerifier[F, Proposition, Proof]
    ): ProofVerifier[F, Propositions.Compositional.Threshold, Proofs.Compositional.Threshold] =
      (
        proposition: Propositions.Compositional.Threshold,
        proof:       Proofs.Compositional.Threshold,
        context:     VerificationContext[F]
      ) =>
        if (proposition.threshold === 0) true.pure[F]
        else if (proposition.threshold > proposition.propositions.size) false.pure[F]
        else if (proof.proofs.isEmpty) false.pure[F]
        // We assume a one-to-one pairing of sub-proposition to sub-proof with the assumption that some of the proofs
        // may be Proofs.False
        else if (proof.proofs.size =!= proposition.propositions.size) false.pure[F]
        else {
          proposition.propositions.toList
            .zip(proof.proofs)
            .foldLeftM(0L) {
              case (successCount, _) if successCount >= proposition.threshold =>
                successCount.pure[F]
              case (successCount, (prop, proof)) =>
                proofVerifier.verifyWith(prop, proof, context).map {
                  case true => successCount + 1
                  case _    => successCount
                }
            }
            .map(_ >= proposition.threshold)
        }

    implicit def andVerifier[F[_]: Monad](implicit
      proofVerifier: ProofVerifier[F, Proposition, Proof]
    ): ProofVerifier[F, Propositions.Compositional.And, Proofs.Compositional.And] =
      (proposition: Propositions.Compositional.And, proof: Proofs.Compositional.And, context: VerificationContext[F]) =>
        proofVerifier
          .verifyWith(proposition.a, proof.a, context)
          .flatMap {
            case true => proofVerifier.verifyWith(proposition.b, proof.b, context)
            case _    => false.pure[F]
          }

    implicit def orVerifier[F[_]: Monad](implicit
      proofVerifier: ProofVerifier[F, Proposition, Proof]
    ): ProofVerifier[F, Propositions.Compositional.Or, Proofs.Compositional.Or] =
      (proposition: Propositions.Compositional.Or, proof: Proofs.Compositional.Or, context: VerificationContext[F]) =>
        proofVerifier
          .verifyWith(proposition.a, proof.a, context)
          .flatMap {
            case false => proofVerifier.verifyWith(proposition.b, proof.b, context)
            case _     => true.pure[F]
          }

    implicit def proofVerifier[F[_]: Monad](implicit
      ed25519:         Ed25519,
      extendedEd25519: ExtendedEd25519
    ): ProofVerifier[F, Proposition, Proof] =
      (proposition, proof, context) =>
        (proposition, proof) match {
          case (prop: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519) =>
            publicKeyCurve25519[F].verifyWith(prop, proof, context)
          case (prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519) =>
            publicKeyEd25519[F].verifyWith(prop, proof, context)
          case (prop: Propositions.Knowledge.ExtendedEd25519, proof: Proofs.Knowledge.Ed25519) =>
            publicKeyExtendedEd25519[F].verifyWith(prop, proof, context)
          case (prop: Propositions.Compositional.Threshold, proof: Proofs.Compositional.Threshold) =>
            implicit def v: ProofVerifier[F, Proposition, Proof] = proofVerifier[F]
            thresholdVerifier[F].verifyWith(prop, proof, context)
          case (prop: Propositions.Compositional.And, proof: Proofs.Compositional.And) =>
            implicit def v: ProofVerifier[F, Proposition, Proof] = proofVerifier[F]
            andVerifier[F].verifyWith(prop, proof, context)
          case (prop: Propositions.Compositional.Or, proof: Proofs.Compositional.Or) =>
            implicit def v: ProofVerifier[F, Proposition, Proof] = proofVerifier[F]
            orVerifier[F].verifyWith(prop, proof, context)
          case (prop: Propositions.Contextual.HeightLock, proof: Proofs.Contextual.HeightLock) =>
            heightLockVerifier[F].verifyWith(prop, proof, context)
          case _ =>
            false.pure[F]
        }
  }

  object Instances extends Instances
}

trait VerificationContext[F[_]] {
  def currentTransaction: Transaction
  def heightOfBlock(id: TypedIdentifier): F[Option[Long]]
  def slotOfBlock(id:   TypedIdentifier): F[Option[Slot]]
}
