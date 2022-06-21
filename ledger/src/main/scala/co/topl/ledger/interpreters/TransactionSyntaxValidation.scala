package co.topl.ledger.interpreters

import cats.data.{Chain, NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras._
import co.topl.ledger.models._
import co.topl.models.{Box, Transaction}
import co.topl.typeclasses.implicits._

object TransactionSyntaxValidation {

  def make[F[_]: Sync]: F[TransactionSyntaxValidationAlgebra[F]] =
    Sync[F].delay(
      (
        transaction => Sync[F].delay(validators.foldMap(_.apply(transaction)).as(transaction))
      ): TransactionSyntaxValidationAlgebra[F]
    )

  private[interpreters] val validators: Chain[Transaction => ValidatedNec[TransactionSyntaxError, Unit]] =
    Chain(
      nonEmptyInputsValidation,
      positiveTimestampValidation,
      positiveOutputValuesValidation,
      sufficientFundsValidation
    )

  /**
   * Verify that this transaction contains at least one input
   */
  private[interpreters] def nonEmptyInputsValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.inputs.nonEmpty, (), TransactionSyntaxErrors.EmptyInputs)

  /**
   * Verify that the timestamp of the transaction is positive (greater than 0).  Transactions _can_ be created
   * in the past.
   */
  private[interpreters] def positiveTimestampValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    Validated.condNec(transaction.timestamp >= 0, (), TransactionSyntaxErrors.InvalidTimestamp(transaction.timestamp))

  /**
   * Verify that each transaction output contains a positive quantity (where applicable)
   */
  private[interpreters] def positiveOutputValuesValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    transaction.outputs
      .foldMap(output =>
        (output.value match {
          case t: Box.Values.Poly  => t.quantity.data.some
          case t: Box.Values.Arbit => t.quantity.data.some
          case t: Box.Values.Asset => t.quantity.data.some
          case _                   => none
        }).foldMap(quantity =>
          Validated
            .condNec(
              quantity > BigInt(0),
              (),
              TransactionSyntaxErrors.NonPositiveOutputValue(output.value): TransactionSyntaxError
            )
        )
      )

  /**
   * Ensure the input value quantities exceed or equal the (non-minting) output value quantities
   */
  private[interpreters] def sufficientFundsValidation(
    transaction: Transaction
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    quantityBasedValidation(transaction) { f =>
      val filteredInputs = transaction.inputs.map(_.value).filter(f.isDefinedAt)
      val filteredOutputs = transaction.outputs.filterNot(_.minting).map(_.value).filter(f.isDefinedAt)
      val inputsSum = filteredInputs.map(f).sumAll
      val outputsSum = filteredOutputs.map(f).sumAll
      Validated.condNec(
        inputsSum >= outputsSum,
        (),
        TransactionSyntaxErrors.InsufficientInputFunds(filteredInputs, filteredOutputs): TransactionSyntaxError
      )
    }

  /**
   * Perform validation based on the quantities of boxes grouped by type
   * @param f an extractor function which retrieves a BigInt from a Box.Value
   */
  private[interpreters] def quantityBasedValidation(transaction: Transaction)(
    f: PartialFunction[Box.Value, BigInt] => ValidatedNec[TransactionSyntaxError, Unit]
  ): ValidatedNec[TransactionSyntaxError, Unit] =
    NonEmptyChain(
      // Extract all Poly values and their quantities
      f { case Box.Values.Poly(quantity) => quantity.data },
      // Extract all Asset values and their quantities
      f { case Box.Values.Arbit(quantity) => quantity.data }
    ).appendChain(
      // Extract all Asset values (grouped by asset code) and their quantities
      Chain.fromSeq(
        (transaction.inputs.map(_.value) ++ transaction.outputs.map(_.value))
          .collect { case a: Box.Values.Asset =>
            a.assetCode
          }
          .toList
          .distinct
          .map(code => f { case a: Box.Values.Asset if a.assetCode === code => a.quantity.data })
      )
    ).combineAll
}
