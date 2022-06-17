package co.topl.ledger.models

import co.topl.models.{Box, Transaction}

sealed abstract class TransactionSemanticError

object TransactionSemanticErrors {

  /**
   * The declared Transaction Input does not match the data stored on the chain
   */
  case class InputDataMismatch(input: Transaction.Input) extends TransactionSemanticError

  /**
   * The given input Box ID is not currently spendable
   *   - The box may have never existed
   *   - The box may have been spent already
   */
  case class UnspendableBox(boxId: Box.Id) extends TransactionSemanticError
}
