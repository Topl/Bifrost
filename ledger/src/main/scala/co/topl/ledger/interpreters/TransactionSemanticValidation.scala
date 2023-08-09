package co.topl.ledger.interpreters

import cats.data.{EitherT, NonEmptyChain, Validated, ValidatedNec}
import cats.effect._
import cats.implicits._
import cats.{Applicative, Functor, Monad}
import co.topl.brambl.common.ContainsEvidence
import co.topl.brambl.common.ContainsEvidence.blake2bEvidenceFromImmutable
import co.topl.brambl.common.ContainsImmutable.instances._
import co.topl.brambl.models.TransactionId
import co.topl.brambl.models.box.Lock
import co.topl.brambl.models.transaction.{IoTransaction, Schedule, SpentTransactionOutput, UnspentTransactionOutput}
import co.topl.consensus.models.BlockId
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models._

object TransactionSemanticValidation {

  def make[F[_]: Sync](
    fetchTransaction: TransactionId => F[IoTransaction],
    boxState:         BoxStateAlgebra[F]
  ): F[TransactionSemanticValidationAlgebra[F]] =
    Sync[F].delay(
      new TransactionSemanticValidationAlgebra[F] {

        /**
         * Validate the semantics of a transaction by verifying
         *  - for each input, the claimed value/proposition matches the data defined in the spent output
         *  - for each input, the referenced output is still spendable
         *  - for each output, in case of a group constructor token, the utxo referenced should contain LVLs
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
                    (
                      EitherT(scheduleValidation[F](context.slot)(transaction.datum.event.schedule).map(_.toEither)) >>
                      EitherT(dataValidation(fetchTransaction)(input).map(_.toEither)) >>
                      EitherT(spendableValidation(boxState)(context.parentHeaderId)(input).map(_.toEither)) >>
                      EitherT(groupConstructorTokenLvlValidation(fetchTransaction)(transaction.outputs).map(_.toEither))
                    ).toNestedValidatedNec.value
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
    fetchTransaction: TransactionId => F[IoTransaction]
  )(input: SpentTransactionOutput): F[Validated[NonEmptyChain[TransactionSemanticError], Unit]] =
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

  /**
   * Does the box referenced in the input still exist in a "spendable" state?
   */
  private def spendableValidation[F[_]: Monad](
    boxState: BoxStateAlgebra[F]
  )(blockId: BlockId)(input: SpentTransactionOutput): F[ValidatedNec[TransactionSemanticError, Unit]] =
    boxState
      .boxExistsAt(blockId)(input.address)
      .ifM(
        ().validNec[TransactionSemanticError].pure[F],
        (TransactionSemanticErrors.UnspendableBox(input.address): TransactionSemanticError).invalidNec[Unit].pure[F]
      )

  /**
   * Is this Transaction valid at the provided Slot?
   */
  private def scheduleValidation[F[_]: Applicative](
    slot: Slot
  )(schedule: Schedule): F[ValidatedNec[TransactionSemanticError, Unit]] =
    Validated
      .condNec(
        slot >= schedule.min && slot <= schedule.max,
        (),
        TransactionSemanticErrors.UnsatisfiedSchedule(slot, schedule): TransactionSemanticError
      )
      .pure[F]

  /**
   * is this transaction a group constructor token, and the UTXO referenced contains LVLs, that are paid for minting it?
   */
  private def groupConstructorTokenLvlValidation[F[_]: Monad](
    fetchTransaction: TransactionId => F[IoTransaction]
  )(outputs: Seq[UnspentTransactionOutput]): F[ValidatedNec[TransactionSemanticError, Unit]] =
    outputs
      .filter(_.value.value.isGroup)
      .map(_.value.getGroup)
      .forallM[F] { group =>
        fetchTransaction(group.txId).map(_.outputs(group.index)).map(_.value.value.isLvl)
      }
      .ifM(
        ().validNec[TransactionSemanticError].pure[F],
        (TransactionSemanticErrors.UnsatisfiedGroupMismatch(outputs): TransactionSemanticError).invalidNec[Unit].pure[F]
      )

}
