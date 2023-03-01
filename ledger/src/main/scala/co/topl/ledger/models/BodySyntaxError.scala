package co.topl.ledger.models

import cats.data.NonEmptyChain
import cats.data.NonEmptySet
import co.topl.brambl.models.KnownIdentifier
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.models.Box

trait BodySyntaxError

object BodySyntaxErrors {

  case class TransactionSyntaxErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSyntaxError]
  ) extends BodySyntaxError

  case class DoubleSpend(boxIds: NonEmptySet[KnownIdentifier.TransactionOutput32]) extends BodySyntaxError
}
