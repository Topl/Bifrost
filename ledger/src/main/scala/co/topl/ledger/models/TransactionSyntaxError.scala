package co.topl.ledger.models

import cats.data.Chain
import co.topl.models.{Box, Timestamp}

sealed abstract class TransactionSyntaxError

object TransactionSyntaxErrors {
  case object EmptyInputs extends TransactionSyntaxError
  case class InvalidTimestamp(timestamp: Timestamp) extends TransactionSyntaxError
  case class NonPositiveOutputValue(outputValue: Box.Value) extends TransactionSyntaxError
  case class InsufficientInputFunds[V <: Box.Value](inputs: Chain[V], outputs: Chain[V]) extends TransactionSyntaxError
}
