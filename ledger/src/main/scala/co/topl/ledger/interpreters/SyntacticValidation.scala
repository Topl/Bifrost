package co.topl.ledger.interpreters

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.effect.Sync
import cats.implicits._
import co.topl.ledger.algebras.{InvalidSyntaxError, InvalidSyntaxErrors, SyntacticValidationAlgebra}
import co.topl.models.{Box, Transaction}

object SyntacticValidation {

  def make[F[_]: Sync]: F[SyntacticValidationAlgebra[F]] =
    Sync[F].delay(
      (
        transaction =>
          Sync[F].delay(
            NonEmptyChain(
              nonEmptyInputsValidation _,
              positiveTimestampValidation _,
              positiveOutputValuesValidation _,
              sufficientFundsValidation _
            ).ap(transaction.pure[NonEmptyChain])
              .foldMap(_.void)
              .as(transaction)
          )
      ): SyntacticValidationAlgebra[F]
    )

  private def nonEmptyInputsValidation(transaction: Transaction): ValidatedNec[InvalidSyntaxError, Transaction] =
    Validated.condNec(transaction.inputs.nonEmpty, transaction, InvalidSyntaxErrors.EmptyInputs: InvalidSyntaxError)

  private def positiveTimestampValidation(transaction: Transaction): ValidatedNec[InvalidSyntaxError, Transaction] =
    Validated.condNec(
      transaction.timestamp > 0,
      transaction,
      InvalidSyntaxErrors.InvalidTimestamp(transaction.timestamp): InvalidSyntaxError
    )

  private def positiveOutputValuesValidation(
    transaction: Transaction
  ): ValidatedNec[InvalidSyntaxError, Transaction] = {
    def validateByValueType(f: PartialFunction[Box.Value, BigInt]): ValidatedNec[InvalidSyntaxError, Unit] =
      transaction.outputs
        .map(_.value)
        .collect {
          case t if f.isDefinedAt(t) =>
            Validated.condNec(f(t) > BigInt(0), (), InvalidSyntaxErrors.NonPositiveOutputValue(t): InvalidSyntaxError)
        }
        .combineAllOption
        .getOrElse(Validated.validNec(()))

    NonEmptyChain(
      validateByValueType { case Box.Values.Poly(quantity) => quantity.data },
      validateByValueType { case Box.Values.Arbit(quantity) => quantity.data },
      validateByValueType { case a: Box.Values.Asset => a.quantity.data }
    ).combineAll
      .as(transaction)
  }

  private def sufficientFundsValidation(transaction: Transaction): ValidatedNec[InvalidSyntaxError, Transaction] = {
    def validateByValueType(f: PartialFunction[Box.Value, BigInt]): ValidatedNec[InvalidSyntaxError, Unit] = {
      val (in, out) =
        (
          transaction.inputs.map(_.value).filter(f.isDefinedAt),
          transaction.outputs.map(_.value).filter(f.isDefinedAt)
        )
      val outSum = out.map(f).sumAll
      val inSum = in.map(f).sumAll
      Validated.condNec(inSum >= outSum, (), InvalidSyntaxErrors.InsufficientInputFunds(in, out): InvalidSyntaxError)
    }
    NonEmptyChain(
      validateByValueType { case Box.Values.Poly(quantity) => quantity.data },
      validateByValueType { case Box.Values.Arbit(quantity) => quantity.data },
      validateByValueType { case a: Box.Values.Asset => a.quantity.data }
    )
      .foldMap(_.void)
      .as(transaction)
  }
}
