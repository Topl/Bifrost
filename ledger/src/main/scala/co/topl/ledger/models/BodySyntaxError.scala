package co.topl.ledger.models

import cats.data.NonEmptyChain
import cats.data.NonEmptySet
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.TransactionSyntaxError

sealed trait BodySyntaxError

object BodySyntaxErrors {

  case class TransactionSyntaxErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSyntaxError]
  ) extends BodySyntaxError

  case class DoubleSpend(boxIds: NonEmptySet[TransactionOutputAddress]) extends BodySyntaxError
}
