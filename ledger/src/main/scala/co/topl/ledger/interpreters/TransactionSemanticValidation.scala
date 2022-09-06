package co.topl.ledger.interpreters

import cats.data.{EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.effect._
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models._
import co.topl.typeclasses.implicits._

object TransactionSemanticValidation {

  def make[F[_]: Sync](
    fetchTransaction: TypedIdentifier => F[Transaction],
    boxState:         BoxStateAlgebra[F]
  ): F[TransactionSemanticValidationAlgebra[F]] =
    Sync[F].delay(
      new TransactionSemanticValidationAlgebra[F] {

        /**
         * Validate the semantics of a transaction by verifying
         *  - for each input, the claimed value/proposition matches the data defined in the spent output
         *  - for each input, the referenced output is still spendable
         */
        def validate(
          context:     TransactionValidationContext
        )(transaction: Transaction): F[ValidatedNec[TransactionSemanticError, Transaction]] =
          AugmentedBoxState
            .make(boxState)(
              context.prefix.foldLeft(AugmentedBoxState.StateAugmentation.empty)(_.augment(_))
            )
            .flatMap(boxState =>
              transaction.inputs
                // Stop validating after the first error
                .foldM(transaction.validNec[TransactionSemanticError]) {
                  case (Validated.Valid(_), input) =>
                    EitherT(dataValidation(fetchTransaction)(input).map(_.toEither))
                      .flatMapF(_ => spendableValidation(boxState)(context.parentHeaderId)(input).map(_.toEither))
                      .flatMapF(_ => scheduleValidation[F](context.slot)(transaction.schedule).map(_.toEither))
                      .toNestedValidatedNec
                      .value
                      .map(_.leftMap(_.flatten).as(transaction))
                  case (invalid, _) => invalid.pure[F]
                }
            )
      }
    )

  /**
   * Does the box value included on the input of _this_ transaction match the value on the output of the spent transaction?
   *
   * Does the proposition's evidence included on the input of _this_ transaction match the evidence associated
   * with the Spending Address which owns the box being spent?
   */
  private def dataValidation[F[_]: Functor](
    fetchTransaction: TypedIdentifier => F[Transaction]
  )(input:            Transaction.Input): F[Validated[NonEmptyChain[TransactionSemanticError], Unit]] =
    fetchTransaction(input.boxId.transactionId)
      .map(spentTransaction =>
        // Did the output referenced by this input ever exist?  (Not a spend-ability check, just existence)
        spentTransaction.outputs
          .get(input.boxId.transactionOutputIndex)
          .toValidNec[TransactionSemanticError](TransactionSemanticErrors.UnspendableBox(input.boxId))
          .ensure(NonEmptyChain(TransactionSemanticErrors.InputDataMismatch(input)))(spentOutput =>
            // Does the box value claimed on the input of _this_ transaction match the
            // box value (in state) from the spent output?
            spentOutput.value === input.value &&
            // Does the proposition claimed in the input contain the same evidence that is defined on the
            // spent output's address?
            spentOutput.address.spendingAddress.typedEvidence === input.proposition.typedEvidence
          )
          .void
      )

  /**
   * Does the box referenced in the input still exist in a "spendable" state?
   */
  private def spendableValidation[F[_]: Monad](
    boxState: BoxStateAlgebra[F]
  )(blockId:  TypedIdentifier)(input: Transaction.Input): F[ValidatedNec[TransactionSemanticError, Unit]] =
    boxState
      .boxExistsAt(blockId)(input.boxId)
      .ifM(
        ().validNec[TransactionSemanticError].pure[F],
        (TransactionSemanticErrors.UnspendableBox(input.boxId): TransactionSemanticError).invalidNec[Unit].pure[F]
      )

  /**
   * Is this Transaction valid at the provided Slot?
   */
  private def scheduleValidation[F[_]: Applicative](
    slot:     Slot
  )(schedule: Transaction.Schedule): F[ValidatedNec[TransactionSemanticError, Unit]] =
    Validated
      .condNec(
        slot >= schedule.minimumSlot && slot <= schedule.maximumSlot,
        (),
        TransactionSemanticErrors.UnsatisfiedSchedule(slot, schedule): TransactionSemanticError
      )
      .pure[F]

}
