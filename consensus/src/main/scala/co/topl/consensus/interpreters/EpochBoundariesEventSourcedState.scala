package co.topl.consensus.interpreters

import cats.effect.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.eventtree.{EventSourcedState, ParentChildTree}
import co.topl.models._

/**
 * An EventSourcedState which operates on an `EpochBoundaries`.
 *
 * Applying a block depends on whether or not the block crosses an epoch boundary.  If the block crosses an epoch
 * boundary, its parent is added to the EpochBoundaries store.  If the block does not cross an epoch boundary,
 * no operation takes place.
 *
 * Unapplying a block depends on whether or not the block crosses an epoch boundary.  If the block crosses an epoch
 * boundary, its parent is removed from the EpochBoundaries store.  If the block does not cross an epoch boundary,
 * no operation takes place.
 */
object EpochBoundariesEventSourcedState {

  // Captures the _last_ block ID of each epoch
  type EpochBoundaries[F[_]] = Store[F, Epoch, TypedIdentifier]

  def make[F[_]: Async](
    clock:           ClockAlgebra[F],
    currentBlockId:  F[TypedIdentifier],
    parentChildTree: ParentChildTree[F, TypedIdentifier],
    initialState:    F[EpochBoundaries[F]],
    fetchSlotData:   TypedIdentifier => F[SlotData]
  ): F[EventSourcedState[F, EpochBoundaries[F]]] = {
    def applyBlock(state: EpochBoundaries[F], blockId: TypedIdentifier) =
      for {
        slotData    <- fetchSlotData(blockId)
        epoch       <- clock.epochOf(slotData.slotId.slot)
        parentEpoch <- clock.epochOf(slotData.parentSlotId.slot)
        newState <- (epoch === parentEpoch)
          .pure[F]
          .ifM(
            ifTrue = state.pure[F],
            ifFalse = state.put(parentEpoch, slotData.parentSlotId.blockId).as(state)
          )
      } yield newState

    def unapplyBlock(state: EpochBoundaries[F], blockId: TypedIdentifier) =
      for {
        slotData    <- fetchSlotData(blockId)
        epoch       <- clock.epochOf(slotData.slotId.slot)
        parentEpoch <- clock.epochOf(slotData.parentSlotId.slot)
        newState <- (epoch === parentEpoch)
          .pure[F]
          .ifM(
            ifTrue = state.pure[F],
            ifFalse = state.remove(parentEpoch).as(state)
          )
      } yield newState

    EventSourcedState.OfTree.make(
      initialState = initialState,
      initialEventId = currentBlockId,
      applyEvent = applyBlock,
      unapplyEvent = unapplyBlock,
      parentChildTree = parentChildTree
    )
  }
}
