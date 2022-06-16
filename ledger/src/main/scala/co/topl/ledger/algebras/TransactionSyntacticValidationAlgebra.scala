package co.topl.ledger.algebras

import cats.data.Chain
import co.topl.algebras.ContextlessValidation
import co.topl.models.{Box, Timestamp, Transaction}

trait TransactionSyntacticValidationAlgebra[F[_]] extends ContextlessValidation[F, InvalidSyntaxError, Transaction]

sealed abstract class InvalidSyntaxError

object InvalidSyntaxErrors {
  case object EmptyInputs extends InvalidSyntaxError
  case class InvalidTimestamp(timestamp: Timestamp) extends InvalidSyntaxError
  case class NonPositiveOutputValue(outputValue: Box.Value) extends InvalidSyntaxError
  case class InsufficientInputFunds[V <: Box.Value](inputs: Chain[V], outputs: Chain[V]) extends InvalidSyntaxError
}
