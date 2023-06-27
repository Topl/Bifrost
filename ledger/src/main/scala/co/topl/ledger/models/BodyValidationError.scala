package co.topl.ledger.models

import cats.data.{NonEmptyChain, NonEmptySet}
import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.{TransactionAuthorizationError, TransactionSyntaxError}

sealed abstract class BodyValidationError

sealed abstract class BodyAuthorizationError extends BodyValidationError

object BodyAuthorizationErrors {

  case class TransactionAuthorizationErrors(
    transaction:        IoTransaction,
    authorizationError: TransactionAuthorizationError
  ) extends BodyAuthorizationError
}

sealed abstract class BodySemanticError extends BodyValidationError

object BodySemanticErrors {

  case class TransactionSemanticErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSemanticError]
  ) extends BodySemanticError

  case class TransactionRegistrationError(transaction: IoTransaction) extends BodySemanticError
}

sealed trait BodySyntaxError extends BodyValidationError

object BodySyntaxErrors {

  case class TransactionSyntaxErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSyntaxError]
  ) extends BodySyntaxError

  case class DoubleSpend(boxIds: NonEmptySet[TransactionOutputAddress]) extends BodySyntaxError
}
