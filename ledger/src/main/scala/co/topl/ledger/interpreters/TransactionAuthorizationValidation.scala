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

    def validate(blockId: TypedIdentifier)(
      transaction:        Transaction
    ): F[ValidatedNec[TransactionAuthorizationError, Transaction]] =
      transaction.inputs
        .foldMapM(input => verifyProof(blockId)(transaction)(input.proposition, input.proof))
        .map(_.as(transaction))

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
        case (proposition: Propositions.Contextual.RequiredBoxState, _: Proofs.Contextual.RequiredBoxState) =>
          validateContextualRequiredBoxState(transaction)(proposition)

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
        .use(_.hash(proof.salt.data :+ proof.value).pure[F])
        .map(hashed =>
          Validated.condNec(
            hashed.data === proposition.digest.data,
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
        .leftMap(errors =>
          if (errors.containsContextualError)
            NonEmptyChain[TransactionAuthorizationError](TransactionAuthorizationErrors.Contextual(proposition, proof))
          else
            NonEmptyChain[TransactionAuthorizationError](TransactionAuthorizationErrors.Permanent(proposition, proof))
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
      ).leftFlatMap(errors =>
        EitherT(verifyProof(blockId)(transaction)(proposition.b, proof.b).map(_.toEither))
          .leftMap { bErrors =>
            val error =
              if (errors.concat(bErrors).containsContextualError)
                TransactionAuthorizationErrors.Contextual(proposition, proof)
              else
                TransactionAuthorizationErrors.Permanent(proposition, proof)
            NonEmptyChain(error: TransactionAuthorizationError)
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
        .foldLeftM((0, false)) {
          case ((successCount, contextualErrorFound), (subProposition, subProof))
              if successCount < proposition.threshold =>
            verifyProof(blockId)(transaction)(subProposition, subProof)
              .map {
                case Validated.Valid(_) =>
                  (successCount + 1, contextualErrorFound)
                case Validated.Invalid(errors) =>
                  (
                    successCount,
                    contextualErrorFound && errors.containsContextualError
                  )
              }
          case (res, _) => res.pure[F]
        }
        .map {
          case (successCount, _) if successCount >= proposition.threshold => ().validNec
          case (_, true) =>
            (TransactionAuthorizationErrors.Contextual(proposition, proof): TransactionAuthorizationError)
              .invalidNec[Unit]
          case _ =>
            (TransactionAuthorizationErrors.Permanent(proposition, proof): TransactionAuthorizationError)
              .invalidNec[Unit]
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
              NonEmptyChain(
                TransactionAuthorizationErrors.Contextual(proposition, proof): TransactionAuthorizationError
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
            height <= proposition.height,
            (),
            TransactionAuthorizationErrors
              .Permanent(proposition, Proofs.Contextual.HeightLock()): TransactionAuthorizationError
          )
        )

    private def validateContextualRequiredBoxState(transaction: Transaction)(
      proposition:                                              Propositions.Contextual.RequiredBoxState
    ): F[ValidatedNec[TransactionAuthorizationError, Unit]] = {
      val fetchBox = proposition.location match {
        case BoxLocations.Input =>
          (index: Short) =>
            transaction.inputs.get(index.toLong).map(input => Box(input.proposition.typedEvidence, input.value))
        case BoxLocations.Output =>
          (index: Short) =>
            transaction.outputs
              .get(index.toLong)
              .map(output => Box(output.address.spendingAddress.typedEvidence, output.value))
      }
      proposition.boxes
        .foldMap { case (index, expectedBox) =>
          Validated.condNec(
            fetchBox(index).contains(expectedBox),
            (),
            TransactionAuthorizationErrors
              .Permanent(proposition, Proofs.Contextual.RequiredBoxState()): TransactionAuthorizationError
          )
        }
        .pure[F]
    }

    implicit private class ErrorsSupport[G[_]: UnorderedFoldable](errors: G[TransactionAuthorizationError]) {

      def containsContextualError: Boolean = errors.exists {
        case _: TransactionAuthorizationErrors.Contextual => true
        case _                                            => false
      }
    }
  }
}
