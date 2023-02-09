package co.topl.ledger.models

import cats.data.NonEmptyChain
import co.topl.models.Transaction

sealed abstract class BodySemanticError

object BodySemanticErrors {

  case class TransactionSemanticErrors(
    transaction:    Transaction,
    semanticErrors: NonEmptyChain[TransactionSemanticError]
  ) extends BodySemanticError
}
