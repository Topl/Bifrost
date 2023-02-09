package co.topl.ledger.models

import cats.data.Chain
import co.topl.models._

sealed abstract class TransactionSyntaxError

object TransactionSyntaxErrors {
  case object EmptyInputs extends TransactionSyntaxError
  case class DuplicateInput(boxId: Box.Id) extends TransactionSyntaxError
  case object ExcessiveOutputsCount extends TransactionSyntaxError
  case class InvalidTimestamp(timestamp: Timestamp) extends TransactionSyntaxError

  case class InvalidSchedule(schedule: Transaction.Schedule) extends TransactionSyntaxError
  case class NonPositiveOutputValue(outputValue: Box.Value) extends TransactionSyntaxError
  case class InsufficientInputFunds[V <: Box.Value](inputs: Chain[V], outputs: Chain[V]) extends TransactionSyntaxError

  case class InvalidProofType(proposition: Proposition, proof: Proof) extends TransactionSyntaxError
  case object InvalidDataLength extends TransactionSyntaxError
}
