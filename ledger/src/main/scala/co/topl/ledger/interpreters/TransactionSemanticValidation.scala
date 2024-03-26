package co.topl.ledger.interpreters

import cats.Monad
import cats.data.{EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.effect._
import cats.implicits._
import co.topl.algebras.ContextlessValidationAlgebra
import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsEvidence.blake2bEvidenceFromImmutable
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput}
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models._

object TransactionSemanticValidation {

  def make[F[_]: Async](
    fetchTransaction: TransactionId => F[IoTransaction],
    boxState:         BoxStateAlgebra[F]
  ): Resource[F, TransactionSemanticValidationAlgebra[F]] =
    (makeDataValidation(fetchTransaction), makeContextualValidation(boxState))
      .mapN((v1, v2) =>
        new TransactionSemanticValidationAlgebra[F] {

          override def validate(
            context: TransactionValidationContext
          )(t: IoTransaction): F[ValidatedNec[TransactionSemanticError, IoTransaction]] =
            v1.validate(t).flatMap {
              case Validated.Valid(_) => v2.validate(context)(t)
              case e                  => e.pure[F]
            }
        }
      )

  def makeDataValidation[F[_]: Monad](
    fetchTransaction: TransactionId => F[IoTransaction]
  ): Resource[F, TransactionSemanticDataValidation[F]] =
    Resource.pure(new TransactionSemanticDataValidation(fetchTransaction))

  def makeContextualValidation[F[_]: Async](
    boxState: BoxStateAlgebra[F]
  ): Resource[F, TransactionSemanticContextualValidation[F]] =
    Resource.pure(new TransactionSemanticContextualValidation(boxState))

}

/**
 * Verifies that each claimed input value+attestation matches the information saved in the referenced output
 */
class TransactionSemanticDataValidation[F[_]: Monad](
  fetchTransaction: TransactionId => F[IoTransaction]
) extends ContextlessValidationAlgebra[F, TransactionSemanticError, IoTransaction] {

  /**
   * Determines the validity of the given value, scoped without any contextual information
   * (i.e. if T is a Transaction, there is no context about previous transactions or blocks)
   * Usually used for syntactic validation purposes.
   */
  override def validate(transaction: IoTransaction): F[ValidatedNec[TransactionSemanticError, IoTransaction]] =
    transaction.inputs
      // Stop validating after the first error
      .foldM(transaction.validNec[TransactionSemanticError]) {
        case (Validated.Valid(_), input) =>
          dataValidation(input).map(_.as(transaction))
        case (e, _) =>
          e.pure[F]
      }

  /**
   * Does the box value included on the input of _this_ transaction match the value on the output of the spent transaction?
   *
   * Does the proposition's evidence included on the input of _this_ transaction match the evidence associated
   * with the Spending Address which owns the box being spent?
   */
  private def dataValidation(
    input: SpentTransactionOutput
  ): F[Validated[NonEmptyChain[TransactionSemanticError], Unit]] =
    fetchTransaction(input.address.id)
      .map(spentTransaction =>
        // Did the output referenced by this input ever exist?  (Not a spend-ability check, just existence)
        spentTransaction.outputs
          .get(input.address.index)
          .toValidNec[TransactionSemanticError](TransactionSemanticErrors.UnspendableBox(input.address))
          .ensure(NonEmptyChain(TransactionSemanticErrors.InputDataMismatch(input)))(spentOutput =>
            // Does the box value claimed on the input of _this_ transaction match the
            // box value (in state) from the spent output?
            spentOutput.value == input.value &&
            // Does the proposition claimed in the input contain the same evidence that is defined on the
            // spent output's address?
            spentOutput.address.id.value == ContainsEvidence[Lock.Predicate]
              .sizedEvidence(input.attestation.getPredicate.lock)
              .digest
              .value
          )
          .void
      )
}

class TransactionSemanticContextualValidation[F[_]: Async](boxState: BoxStateAlgebra[F])
    extends TransactionSemanticValidationAlgebra[F] {

  /**
   * Validate the semantics of a transaction by verifying
   *  - for each input, the claimed value/proposition matches the data defined in the spent output
   *  - for each input, the referenced output is still spendable
   */
  def validate(
    context: TransactionValidationContext
  )(transaction: IoTransaction): F[ValidatedNec[TransactionSemanticError, IoTransaction]] =
    AugmentedBoxState
      .make(boxState)(
        context.prefix.foldLeft(AugmentedBoxState.StateAugmentation.empty)(_.augment(_))
      )
      .flatMap(boxState =>
        transaction.inputs
          // Stop validating after the first error
          .foldM(transaction.validNec[TransactionSemanticError]) {
            case (Validated.Valid(_), input) =>
              Async[F].cede >>
              (
                EitherT(scheduleValidation(context.slot)(transaction.datum.event.schedule).map(_.toEither)) >>
                EitherT(spendableValidation(boxState)(context.parentHeaderId)(input).map(_.toEither))
              ).toNestedValidatedNec.value
                .map(_.leftMap(_.flatten).as(transaction))
            case (invalid, _) => invalid.pure[F]
          }
      )

  /**
   * Does the box referenced in the input still exist in a "spendable" state?
   */
  private def spendableValidation(boxState: BoxStateAlgebra[F])(
    blockId: BlockId
  )(input: SpentTransactionOutput): F[ValidatedNec[TransactionSemanticError, Unit]] =
    boxState
      .boxExistsAt(blockId)(input.address)
      .ifM(
        ().validNec[TransactionSemanticError].pure[F],
        (TransactionSemanticErrors.UnspendableBox(input.address): TransactionSemanticError).invalidNec[Unit].pure[F]
      )

  /**
   * Is this Transaction valid at the provided Slot?
   */
  private def scheduleValidation(
    slot: Slot
  )(schedule: Schedule): F[ValidatedNec[TransactionSemanticError, Unit]] =
    Validated
      .condNec(
        slot >= schedule.min && slot <= schedule.max,
        (),
        TransactionSemanticErrors.UnsatisfiedSchedule(slot, schedule): TransactionSemanticError
      )
      .pure[F]
}
