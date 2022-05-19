package co.topl.ledger.interpreters

import cats.implicits._
import cats.data.{Validated, ValidatedNec}
import cats.effect.Sync
import co.topl.ledger.algebras.InvalidSyntaxErrors.NonPositiveOutputValue
import co.topl.ledger.algebras.{InvalidSyntaxError, InvalidSyntaxErrors, SyntacticValidationAlgebra}
import co.topl.models.{Box, Transaction}

object SyntacticValidation {

  def make[F[_]: Sync]: F[SyntacticValidationAlgebra[F]] =
    Sync[F].delay(
      (
        transaction =>
          Sync[F].delay(
            (
              nonEmptyInputsValidation(transaction),
              positiveTimestampValidation(transaction),
              positiveOutputValuesValidation(transaction)
            ).tupled
              .as(transaction)
          )
      ): SyntacticValidationAlgebra[F]
    )

  private def nonEmptyInputsValidation(transaction: Transaction) =
    Validated.condNec(transaction.inputs.nonEmpty, transaction, InvalidSyntaxErrors.EmptyInputs: InvalidSyntaxError)

  private def positiveTimestampValidation(transaction: Transaction) =
    Validated.condNec(
      transaction.timestamp > 0,
      transaction,
      InvalidSyntaxErrors.InvalidTimestamp(transaction.timestamp): InvalidSyntaxError
    )

  private def positiveOutputValuesValidation(transaction: Transaction): ValidatedNec[InvalidSyntaxError, Transaction] =
    transaction.outputs
      .map(output =>
        output.value match {
          case Box.Values.Poly(quantity) =>
            Validated.condNec(
              quantity.data > BigInt(0L),
              (),
              NonPositiveOutputValue(output.value): InvalidSyntaxError
            )
          case Box.Values.Arbit(quantity) =>
            Validated.condNec(
              quantity.data > BigInt(0L),
              (),
              NonPositiveOutputValue(output.value): InvalidSyntaxError
            )
          case Box.Values.Asset(quantity, _, _, _) =>
            Validated.condNec(
              quantity.data > BigInt(0L),
              (),
              NonPositiveOutputValue(output.value): InvalidSyntaxError
            )
          case _ =>
            Validated.validNec(())
        }
      )
      .combineAll
      .as(transaction)
}
