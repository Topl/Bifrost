package co.topl.ledger.models

import co.topl.brambl.models.transaction.IoTransaction
import co.topl.consensus.models.BlockId
import co.topl.models.Slot

/**
 * The context to use when validating the semantics of a Transaction
 */
trait TransactionValidationContext {

  /**
   * The ID of the ancestor of the block being validated (i.e. if validating a transaction in block B, pass in block A)
   */
  def parentHeaderId: BlockId

  /**
   * The sequence of transactions that have already been validated within the current block
   */
  def prefix: Seq[IoTransaction]

  /**
   * The height of the chain for validation purposes
   */
  def height: Long

  /**
   * The slot of the chain for validation purposes
   */
  def slot: Long
}

case class StaticTransactionValidationContext(
  parentHeaderId: BlockId,
  prefix:         Seq[IoTransaction],
  height:         Long,
  slot:           Slot
) extends TransactionValidationContext
