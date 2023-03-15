package co.topl.ledger.models

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.brambl.validation.TransactionAuthorizationError

sealed abstract class BodyAuthorizationError

object BodyAuthorizationErrors {

  case class TransactionAuthorizationErrors(
    transaction:        IoTransaction,
    authorizationError: TransactionAuthorizationError
  ) extends BodyAuthorizationError
}
