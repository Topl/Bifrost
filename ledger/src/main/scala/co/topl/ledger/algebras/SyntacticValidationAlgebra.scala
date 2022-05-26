package co.topl.ledger.algebras

import cats.data.{Chain, ValidatedNec}
import co.topl.models.{Box, Timestamp, Transaction}

trait SyntacticValidationAlgebra[F[_]] {

  /**
   * Determines the syntactic integrity of the given transaction
   */
  def validateSyntax(transaction: Transaction): F[ValidatedNec[InvalidSyntaxError, Transaction]]
}

sealed abstract class InvalidSyntaxError

object InvalidSyntaxErrors {
  case object EmptyInputs extends InvalidSyntaxError
  case class InvalidTimestamp(timestamp: Timestamp) extends InvalidSyntaxError
  case class NonPositiveOutputValue(outputValue: Box.Value) extends InvalidSyntaxError
  case class InsufficientInputFunds[V <: Box.Value](inputs: Chain[V], outputs: Chain[V]) extends InvalidSyntaxError
}
