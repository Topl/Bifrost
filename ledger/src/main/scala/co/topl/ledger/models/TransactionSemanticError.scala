package co.topl.ledger.models

import co.topl.brambl.models.TransactionOutputAddress
import co.topl.brambl.models.transaction.Schedule
import co.topl.brambl.models.transaction.SpentTransactionOutput
import co.topl.models.Slot

sealed abstract class TransactionSemanticError

object TransactionSemanticErrors {

  /**
   * The declared Transaction Input does not match the data stored on the chain
   */
  case class InputDataMismatch(input: SpentTransactionOutput) extends TransactionSemanticError

  /**
   * The given input Box ID is not currently spendable
   *   - The box may have never existed
   *   - The box may have been spent already
   */
  case class UnspendableBox(boxId: TransactionOutputAddress) extends TransactionSemanticError

  /**
   * The Transaction was included in a block with a Slot that does not satisfy the Transaction's schedule requirements
   * @param slot the slot of the block containing the transaction
   * @param schedule the transaction's schedule
   */
  case class UnsatisfiedSchedule(slot: Slot, schedule: Schedule) extends TransactionSemanticError
}
