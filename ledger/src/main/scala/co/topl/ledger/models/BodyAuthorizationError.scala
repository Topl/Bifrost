package co.topl.ledger.models

import cats.data.NonEmptyChain
import co.topl.models.Transaction

sealed abstract class BodyAuthorizationError

object BodyAuthorizationErrors {

  case class TransactionAuthorizationErrors(
    transaction:         Transaction,
    authorizationErrors: NonEmptyChain[TransactionAuthorizationError]
  ) extends BodyAuthorizationError
}
