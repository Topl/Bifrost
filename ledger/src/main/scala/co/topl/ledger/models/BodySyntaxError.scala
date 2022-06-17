package co.topl.ledger.models

import cats.data.NonEmptyChain
import co.topl.models.Transaction

trait BodySyntaxError

object BodySyntaxErrors {

  case class TransactionSyntaxErrors(
    transaction:    Transaction,
    semanticErrors: NonEmptyChain[TransactionSyntaxError]
  )
}
