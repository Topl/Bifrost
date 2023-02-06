package co.topl.ledger.models

import co.topl.models.{Slot, TypedIdentifier}

/**
 * The context to use when validating the semantics of a Transaction
 */
trait BodyValidationContext {

  /**
   * The ID of the ancestor of the block being validated (i.e. if validating a transaction in block B, pass in block A)
   */
  def parentHeaderId: TypedIdentifier

  /**
   * The height of the chain for validation purposes
   */
  def height: Long

  /**
   * The slot of the chain for validation purposes
   */
  def slot: Long
}

case class StaticBodyValidationContext(
  parentHeaderId: TypedIdentifier,
  height:         Long,
  slot:           Slot
) extends BodyValidationContext
