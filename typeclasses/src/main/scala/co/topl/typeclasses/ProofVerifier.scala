package co.topl.typeclasses

import cats._
import cats.implicits._
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.models._
import co.topl.typeclasses.implicits._

import scala.language.implicitConversions

trait ProofVerifier[F[_]] {

  /**
   * Does the given `proof` satisfy the given `proposition` using the given `data`?
   */
  def verifyWith(proposition: Proposition, proof: Proof, context: VerificationContext[F]): F[Boolean]
}

object ProofVerifier {

  trait Implicits {

    implicit class ProofOps(proof: Proof) {

      def satisfies[F[_]](
        proposition:      Proposition
      )(implicit context: VerificationContext[F], ev: ProofVerifier[F]): F[Boolean] =
        ev.verifyWith(proposition, proof, context)
    }

    implicit class PropositionOps(proposition: Proposition) {

      def isSatisifiedBy[F[_]](
        proof:            Proof
      )(implicit context: VerificationContext[F], ev: ProofVerifier[F]): F[Boolean] =
        ev.verifyWith(proposition, proof, context)
    }
  }

  object ops extends Implicits

  trait Instances {
    private val curve25519 = new Curve25519()

    private def publicKeyCurve25519Verifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.Curve25519,
      proof:       Proofs.Knowledge.Curve25519,
      context:     VerificationContext[F]
    ): F[Boolean] =
      curve25519
        .verify(
          proof,
          context.currentTransaction.signableBytes,
          proposition.key
        )
        .pure[F]

    implicit def publicKeyEd25519Verifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.Ed25519,
      proof:       Proofs.Knowledge.Ed25519,
      context:     VerificationContext[F]
    )(implicit
      ed25519: Ed25519
    ): F[Boolean] =
      ed25519
        .verify(
          proof,
          context.currentTransaction.signableBytes,
          proposition.key
        )
        .pure[F]

    private def publicKeyExtendedEd25519Verifier[F[_]: Applicative](
      proposition: Propositions.Knowledge.ExtendedEd25519,
      proof:       Proofs.Knowledge.Ed25519,
      context:     VerificationContext[F]
    )(implicit
      extendedEd25519: ExtendedEd25519
    ): F[Boolean] =
      extendedEd25519
        .verify(
          proof,
          context.currentTransaction.signableBytes,
          proposition.key
        )
        .pure[F]

    private def heightLockVerifier[F[_]: Applicative](
      proposition: Propositions.Contextual.HeightLock,
      context:     VerificationContext[F]
    ): F[Boolean] = (context.currentHeight >= proposition.height).pure[F]

    private def thresholdVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.Threshold,
      proof:       Proofs.Compositional.Threshold,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      if (proposition.threshold === 0) true.pure[F]
      else if (proposition.threshold >= proposition.propositions.size) false.pure[F]
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

    private def andVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.And,
      proof:       Proofs.Compositional.And,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      proofVerifier
        .verifyWith(proposition.a, proof.a, context)
        .flatMap {
          case true => proofVerifier.verifyWith(proposition.b, proof.b, context)
          case _    => false.pure[F]
        }

    private def orVerifier[F[_]: Monad](
      proposition: Propositions.Compositional.Or,
      proof:       Proofs.Compositional.Or,
      context:     VerificationContext[F]
    )(implicit
      proofVerifier: ProofVerifier[F]
    ): F[Boolean] =
      proofVerifier
        .verifyWith(proposition.a, proof.a, context)
        .flatMap {
          case false => proofVerifier.verifyWith(proposition.b, proof.b, context)
          case _     => true.pure[F]
        }

    implicit def proofVerifier[F[_]: Monad](implicit
      ed25519:         Ed25519,
      extendedEd25519: ExtendedEd25519
    ): ProofVerifier[F] =
      (proposition, proof, context) =>
        (proposition, proof) match {
          case (Propositions.PermanentlyLocked, _) =>
            false.pure[F]
          case (prop: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519) =>
            publicKeyCurve25519Verifier[F](prop, proof, context)
          case (prop: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519) =>
            publicKeyEd25519Verifier[F](prop, proof, context)
          case (prop: Propositions.Knowledge.ExtendedEd25519, proof: Proofs.Knowledge.Ed25519) =>
            publicKeyExtendedEd25519Verifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.Threshold, proof: Proofs.Compositional.Threshold) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            thresholdVerifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.And, proof: Proofs.Compositional.And) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            andVerifier[F](prop, proof, context)
          case (prop: Propositions.Compositional.Or, proof: Proofs.Compositional.Or) =>
            implicit def v: ProofVerifier[F] = proofVerifier[F]
            orVerifier[F](prop, proof, context)
          case (prop: Propositions.Contextual.HeightLock, proof: Proofs.Contextual.HeightLock) =>
            heightLockVerifier[F](prop, context)
          case _ =>
            false.pure[F]
        }
  }

  object Instances extends Instances
}

trait VerificationContext[F[_]] {
  def currentTransaction: Transaction
  def currentHeight: Long
}
