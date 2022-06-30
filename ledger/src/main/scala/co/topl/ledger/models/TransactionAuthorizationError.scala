package co.topl.ledger.models

import co.topl.models.{Proof, Proposition}

sealed abstract class TransactionAuthorizationError

object TransactionAuthorizationErrors {
  case class Contextual(proposition: Proposition, proof: Proof) extends TransactionAuthorizationError
  case class Permanent(proposition: Proposition, proof: Proof) extends TransactionAuthorizationError
}
