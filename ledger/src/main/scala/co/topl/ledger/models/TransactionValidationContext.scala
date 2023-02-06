package co.topl.ledger.models

import cats.data.Chain
import co.topl.models.{Slot, Transaction, TypedIdentifier}

/**
 * The context to use when validating the semantics of a Transaction
 */
trait TransactionValidationContext {

  /**
   * The ID of the ancestor of the block being validated (i.e. if validating a transaction in block B, pass in block A)
   */
  def parentHeaderId: TypedIdentifier

  /**
   * The sequence of transactions that have already been validated within the current block
   */
  def prefix: Chain[Transaction]

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
  parentHeaderId: TypedIdentifier,
  prefix:         Chain[Transaction],
  height:         Long,
  slot:           Slot
) extends TransactionValidationContext
