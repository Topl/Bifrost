package co.topl.ledger.algebras

import cats.data.ValidatedNec
import co.topl.algebras.ContextualValidationAlgebra
import co.topl.models.{Box, Transaction, TypedIdentifier}

trait TransactionSemanticValidationAlgebra[F[_]]
    extends ContextualValidationAlgebra[F, InvalidSemanticError, Transaction, TypedIdentifier] {

  /**
   * Determines the semantic integrity of the given transaction within the context of the given parentBlockId
   */
  def validateSemantics(parentBlockId: TypedIdentifier)(
    transaction:                       Transaction
  ): F[ValidatedNec[InvalidSemanticError, Transaction]] =
    validate(parentBlockId)(transaction)
}

sealed abstract class InvalidSemanticError

object InvalidSemanticErrors {

  /**
   * The declared Transaction Input does not match the data stored on the chain
   */
  case class InputDataMismatch(input: Transaction.Input) extends InvalidSemanticError

  /**
   * The given input Box ID is not currently spendable
   *   - The box may have never existed
   *   - The box may have been spent already
   */
  case class UnspendableBox(boxId: Box.Id) extends InvalidSemanticError
}
