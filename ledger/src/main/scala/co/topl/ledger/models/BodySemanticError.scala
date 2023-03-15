package co.topl.ledger.models

import cats.data.NonEmptyChain
import co.topl.brambl.models.transaction.IoTransaction

sealed abstract class BodySemanticError

object BodySemanticErrors {

  case class TransactionSemanticErrors(
    transaction:    IoTransaction,
    semanticErrors: NonEmptyChain[TransactionSemanticError]
  ) extends BodySemanticError
}
