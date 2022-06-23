package co.topl.ledger.models

import cats.data.{NonEmptyChain, NonEmptySet}
import co.topl.models.{Box, Transaction}

trait BodySyntaxError

object BodySyntaxErrors {

  case class TransactionSyntaxErrors(
    transaction:    Transaction,
    semanticErrors: NonEmptyChain[TransactionSyntaxError]
  ) extends BodySyntaxError

  case class DoubleSpend(boxIds: NonEmptySet[Box.Id]) extends BodySyntaxError
}
