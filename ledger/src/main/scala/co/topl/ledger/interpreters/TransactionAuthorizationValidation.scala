package co.topl.ledger.interpreters

import cats.UnorderedFoldable
import cats.data.{EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.algebras.UnsafeResource
import co.topl.codecs.bytes.tetra.instances._
import co.topl.codecs.bytes.typeclasses.implicits._
import co.topl.crypto.hash.Blake2b256
import co.topl.crypto.signing.{Curve25519, Ed25519, ExtendedEd25519}
import co.topl.ledger.algebras.TransactionAuthorizationValidationAlgebra
import co.topl.ledger.models.{TransactionAuthorizationError, TransactionAuthorizationErrors}
import co.topl.models._
import co.topl.typeclasses.implicits._

/**
 * Validates that each Input within a Transaction is properly "authorized".  "Authorized" simply means "does the given
 * Proof satisfy the given Proposition?".
 */
object TransactionAuthorizationValidation {

  def make[F[_]: Sync](
    blake2b256Resource:      UnsafeResource[F, Blake2b256],
    curve25519Resource:      UnsafeResource[F, Curve25519],
    ed25519Resource:         UnsafeResource[F, Ed25519],
    extendedEd25519Resource: UnsafeResource[F, ExtendedEd25519],
    fetchSlotData:           TypedIdentifier => F[SlotData]
  ): F[TransactionAuthorizationValidationAlgebra[F]] =
    Sync[F].delay(
      new Impl(blake2b256Resource, curve25519Resource, ed25519Resource, extendedEd25519Resource, fetchSlotData)
    )

  private class Impl[F[_]: Sync](
    blake2b256Resource:      UnsafeResource[F, Blake2b256],
    curve25519Resource:      UnsafeResource[F, Curve25519],
    ed25519Resource:         UnsafeResource[F, Ed25519],
    extendedEd25519Resource: UnsafeResource[F, ExtendedEd25519],
    fetchSlotData:           TypedIdentifier => F[SlotData]
  ) extends TransactionAuthorizationValidationAlgebra[F] {

    /**
     * Verifies each (Proposition, Proof) pair in the given Transaction
     * @param blockId the ID of the block containing this particular Transaction
     */
    def validate(blockId: TypedIdentifier)(
      transaction:        Transaction
    ): F[ValidatedNec[TransactionAuthorizationError, Transaction]] =
      transaction.inputs
        .foldMapM(input => verifyProof(blockId)(transaction)(input.proposition, input.proof))
        .map(_.as(transaction))

    /**
     * Validates that the given Proof satisfies the given Proposition, within the context of the
     * given transaction and block ID
     */
    private def verifyProof(blockId: TypedIdentifier)(
      transaction:                   Transaction
    )(proposition:                   Proposition, proof: Proof): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      (proposition, proof) match {
        case (proposition: Propositions.PermanentlyLocked.type, proof) =>
          validatePermanentlyLocked(proposition, proof)

        case (proposition: Propositions.Knowledge.Curve25519, proof: Proofs.Knowledge.Curve25519) =>
          validateKnowledgeCurve25519(transaction)(proposition, proof)
        case (proposition: Propositions.Knowledge.Ed25519, proof: Proofs.Knowledge.Ed25519) =>
          validateKnowledgeEd25519(transaction)(proposition, proof)
        case (proposition: Propositions.Knowledge.ExtendedEd25519, proof: Proofs.Knowledge.Ed25519) =>
          validateKnowledgeExtendedEd25519(transaction)(proposition, proof)
        case (proposition: Propositions.Knowledge.HashLock, proof: Proofs.Knowledge.HashLock) =>
          validateKnowledgeHashLock(proposition, proof)

        case (proposition: Propositions.Compositional.And, proof: Proofs.Compositional.And) =>
          validateCompositionalAnd(blockId)(transaction)(proposition, proof)
        case (proposition: Propositions.Compositional.Or, proof: Proofs.Compositional.Or) =>
          validateCompositionalOr(blockId)(transaction)(proposition, proof)
        case (proposition: Propositions.Compositional.Threshold, proof: Proofs.Compositional.Threshold) =>
          validateCompositionalThreshold(blockId)(transaction)(proposition, proof)
        case (proposition: Propositions.Compositional.Not, proof: Proofs.Compositional.Not) =>
          validateCompositionalNot(blockId)(transaction)(proposition, proof)

        case (proposition: Propositions.Contextual.HeightLock, _: Proofs.Contextual.HeightLock) =>
          validateContextualHeightLock(blockId)(proposition)
        case (proposition: Propositions.Contextual.RequiredTransactionIO, _: Proofs.Contextual.RequiredTransactionIO) =>
          validateContextualRequiredTransactionIO(transaction)(proposition)

        case _ =>
          (TransactionAuthorizationErrors.Permanent(proposition, proof): TransactionAuthorizationError)
            .invalidNec[Unit]
            .pure[F]
      }

    private def validatePermanentlyLocked(
      proposition: Propositions.PermanentlyLocked.type,
      proof:       Proof
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      (TransactionAuthorizationErrors.Permanent(proposition, proof): TransactionAuthorizationError)
        .invalidNec[Unit]
        .pure[F]

    private def validateKnowledgeCurve25519(transaction: Transaction)(
      proposition:                                       Propositions.Knowledge.Curve25519,
      proof:                                             Proofs.Knowledge.Curve25519
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      curve25519Resource
        .use(_.verify(proof, transaction.signableBytes, proposition.key).pure[F])
        .ifM(
          ().validNec[TransactionAuthorizationError].pure[F],
          (TransactionAuthorizationErrors
            .Permanent(proposition, proof): TransactionAuthorizationError).invalidNec[Unit].pure[F]
        )

    private def validateKnowledgeEd25519(transaction: Transaction)(
      proposition:                                    Propositions.Knowledge.Ed25519,
      proof:                                          Proofs.Knowledge.Ed25519
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      ed25519Resource
        .use(_.verify(proof, transaction.signableBytes, proposition.key).pure[F])
        .ifM(
          ().validNec[TransactionAuthorizationError].pure[F],
          (TransactionAuthorizationErrors
            .Permanent(proposition, proof): TransactionAuthorizationError).invalidNec[Unit].pure[F]
        )

    private def validateKnowledgeExtendedEd25519(transaction: Transaction)(
      proposition:                                            Propositions.Knowledge.ExtendedEd25519,
      proof:                                                  Proofs.Knowledge.Ed25519
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      extendedEd25519Resource
        .use(_.verify(proof, transaction.signableBytes, proposition.key).pure[F])
        .ifM(
          ().validNec[TransactionAuthorizationError].pure[F],
          (TransactionAuthorizationErrors
            .Permanent(proposition, proof): TransactionAuthorizationError).invalidNec[Unit].pure[F]
        )

    private def validateKnowledgeHashLock(
      proposition: Propositions.Knowledge.HashLock,
      proof:       Proofs.Knowledge.HashLock
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      blake2b256Resource
        .use(_.hash(proof.value).pure[F])
        .map(hashed =>
          Validated.condNec(
            hashed.data === proposition.valueDigest.data,
            (),
            (TransactionAuthorizationErrors.Permanent(proposition, proof): TransactionAuthorizationError)
          )
        )

    private def validateCompositionalAnd(blockId: TypedIdentifier)(
      transaction:                                Transaction
    )(
      proposition: Propositions.Compositional.And,
      proof:       Proofs.Compositional.And
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      (
        EitherT(verifyProof(blockId)(transaction)(proposition.a, proof.a).map(_.toEither)) >>
          EitherT(verifyProof(blockId)(transaction)(proposition.b, proof.b).map(_.toEither))
      )
        .leftMap((errors: NonEmptyChain[TransactionAuthorizationError]) =>
          // If either A or B are permanently invalid, then this whole proof is permanently invalid
          if (errors.containsPermanentError)
            NonEmptyChain[TransactionAuthorizationError](TransactionAuthorizationErrors.Permanent(proposition, proof))
          else
            NonEmptyChain[TransactionAuthorizationError](TransactionAuthorizationErrors.Contextual(proposition, proof))
        )
        .toValidated

    private def validateCompositionalOr(blockId: TypedIdentifier)(
      transaction:                               Transaction
    )(
      proposition: Propositions.Compositional.Or,
      proof:       Proofs.Compositional.Or
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      EitherT(
        verifyProof(blockId)(transaction)(proposition.a, proof.a)
          .map(_.toEither)
      ).leftFlatMap(aErrors =>
        EitherT(verifyProof(blockId)(transaction)(proposition.b, proof.b).map(_.toEither))
          .leftMap { bErrors =>
            // If both A and B are permanently invalid, then this whole proof is permanently invalid
            val error =
              if (aErrors.containsPermanentError && bErrors.containsPermanentError)
                TransactionAuthorizationErrors.Permanent(proposition, proof)
              else
                TransactionAuthorizationErrors.Contextual(proposition, proof)
            NonEmptyChain[TransactionAuthorizationError](error)
          }
      ).toValidated

    private def validateCompositionalThreshold(blockId: TypedIdentifier)(
      transaction:                                      Transaction
    )(
      proposition: Propositions.Compositional.Threshold,
      proof:       Proofs.Compositional.Threshold
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      proposition.propositions.toList
        .zip(proof.proofs)
        // (successCount, permanentSubErrorsCount)
        .foldLeftM((0, 0)) {
          case ((successCount, permanentSubErrorsCount), (subProposition, subProof))
              if successCount < proposition.threshold =>
            verifyProof(blockId)(transaction)(subProposition, subProof)
              .map {
                case Validated.Valid(_) =>
                  (successCount + 1, permanentSubErrorsCount)
                case Validated.Invalid(errors) =>
                  (
                    successCount,
                    permanentSubErrorsCount + (if (errors.containsPermanentError) 1 else 0)
                  )
              }
          // The case where we have already satisfied the minimum number of valid sub-proofs for this threshold
          case (res, _) => res.pure[F]
        }
        .map {
          case (successCount, _) if successCount >= proposition.threshold => ().validNec
          case (_, permanentSubErrorsCount) =>
            val marginOfError = proposition.propositions.size - proposition.threshold
            // If there are too many permanent sub-errors, then we know for sure that this threshold will never pass
            val error =
              if (permanentSubErrorsCount > marginOfError)
                TransactionAuthorizationErrors.Permanent(proposition, proof)
              else
                TransactionAuthorizationErrors.Contextual(proposition, proof)
            (error: TransactionAuthorizationError).invalidNec[Unit]
        }

    private def validateCompositionalNot(blockId: TypedIdentifier)(
      transaction:                                Transaction
    )(
      proposition: Propositions.Compositional.Not,
      proof:       Proofs.Compositional.Not
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      verifyProof(blockId)(transaction)(proposition.a, proof.a)
        .map(
          _.swap
            .leftMap(_ =>
              NonEmptyChain[TransactionAuthorizationError](
                TransactionAuthorizationErrors.Contextual(proposition, proof)
              )
            )
            .void
        )

    private def validateContextualHeightLock(blockId: TypedIdentifier)(
      proposition:                                    Propositions.Contextual.HeightLock
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] =
      fetchSlotData(blockId)
        .map(_.height)
        .map(height =>
          Validated.condNec(
            height >= proposition.height,
            (),
            TransactionAuthorizationErrors
              .Contextual(proposition, Proofs.Contextual.HeightLock()): TransactionAuthorizationError
          )
        )

    private def validateContextualRequiredTransactionIO(transaction: Transaction)(
      proposition:                                                   Propositions.Contextual.RequiredTransactionIO
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] = {
      val fetchBoxByLocation: BoxLocation => Option[Box] = {
        case BoxLocations.Input(index) =>
          transaction.inputs.get(index.toLong).map(input => Box(input.proposition.typedEvidence, input.value))
        case BoxLocations.Output(index) =>
          transaction.outputs
            .get(index.toLong)
            .map(output => Box(output.address.spendingAddress.typedEvidence, output.value))
      }
      proposition.boxes
        .foldMap { case (expectedBox, location) =>
          Validated.condNec(
            fetchBoxByLocation(location).contains(expectedBox),
            (),
            TransactionAuthorizationErrors
              .Permanent(proposition, Proofs.Contextual.RequiredTransactionIO()): TransactionAuthorizationError
          )
        }
        .pure[F]
    }

    implicit private class ErrorsSupport[G[_]: UnorderedFoldable](errors: G[TransactionAuthorizationError]) {

      def containsPermanentError: Boolean = errors.exists {
        case _: TransactionAuthorizationErrors.Permanent => true
        case _                                           => false
      }
    }
  }
}
