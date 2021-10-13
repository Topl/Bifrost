package co.topl.consensus

import cats._
import cats.data._
import cats.implicits._
import co.topl.models.Slot
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

object ChainSelection {

  private val standardOrder: Order[NonEmptyVector[SlotData]] =
    Order
      .whenEqual(
        Order.by[NonEmptyVector[SlotData], Int](_.length),
        Order.whenEqual(
          Order.by[NonEmptyVector[SlotData], Slot](_.last.slotId.slot),
          Order.reverse(
            // TODO: Detect hash collisions?
            Order.whenEqual(
              // TODO: Is it okay to convert `rho` (Byte Array) into a BigInt for ordering?
              Order.by[NonEmptyVector[SlotData], BigInt](h => BigInt(h.last.rho.data.toArray)),
              (_, _) => 1 // This may technically violate associativity expectations
            )
          )
        )
      )

  /**
   * Between two tines, determines which one is "better"
   * @param kLookback The number of blocks in the backward-moving window of blocks for longest-chain rule
   * @param sWindow The number of slots of the forward-moving window of blocks for chain-density rule
   */
  def orderT[F[_]: Monad: Logger](
    storage:   SlotDataCache[F],
    kLookback: Long,
    sWindow:   Int
  ): OrderT[F, SlotData] = new OrderT[F, SlotData] {

    override def compare(x: SlotData, y: SlotData): F[Int] =
      if (x === y) 0.pure[F]
      else
        (NonEmptyVector.one(x), NonEmptyVector.one(y), false)
          .iterateWhileM { case (xSegment, ySegment, useChainDensity) =>
            // Prepend/accumulate the next parent header for the xSegment and the ySegment
            for {
              newXSegment <-
                if (xSegment.head.slotId.slot >= ySegment.head.slotId.slot) {
                  if (xSegment.head.slotId.slot === 0L) xSegment.pure[F] // Genesis case
                  else storage.get(xSegment.head.parentSlotId.blockId).map(_ +: xSegment)
                } else xSegment.pure[F]
              newYSegment <-
                if (ySegment.head.slotId.slot >= xSegment.head.slotId.slot) {
                  if (ySegment.head.slotId.slot === 0L) ySegment.pure[F] // Genesis case
                  else storage.get(ySegment.head.parentSlotId.blockId).map(_ +: ySegment)
                } else ySegment.pure[F]
              newUseChainDensity =
                useChainDensity || (newXSegment.length > kLookback) || (newYSegment.length > kLookback)
            } yield (
              if (newUseChainDensity) sliceWithinSWindow(newXSegment) else newXSegment,
              if (newUseChainDensity) sliceWithinSWindow(newYSegment) else newYSegment,
              newUseChainDensity
            )
          } { case (xSegment, ySegment, _) =>
            // Iterate until a common ancestor is found
            xSegment.head != ySegment.head
          }
          .flatTap {
            case (xSegment, ySegment, useDensityRule) if xSegment.length > 1 && ySegment.length > 1 =>
              Logger[F].info(
                s"Performing chain selection" +
                s" tineX=[${xSegment.length}](${xSegment.head.slotId.show}..${xSegment.last.slotId.show})" +
                s" tineY=[${ySegment.length}](${ySegment.head.slotId.show}..${ySegment.last.slotId.show})" +
                s" useDensityRule=$useDensityRule"
              )
            case _ =>
              Applicative[F].unit
          }
          .map {
            case (xSegment, ySegment, true) =>
              // Density comparison
              xSegment.length.compare(ySegment.length)
            case (xSegment, ySegment, _) =>
              standardOrder.compare(xSegment, ySegment)
          }

    /**
     * In cases where a common-ancestor search traces back more than `kLookback` blocks, we only need to aggregate
     * blocks within the forward-traversing sWindow.  The result is used when calculating chain density.
     */
    @tailrec
    private def sliceWithinSWindow(segment: NonEmptyVector[SlotData]): NonEmptyVector[SlotData] =
      if (segment.length == 1) segment
      else if (segment.last.slotId.slot - segment.head.slotId.slot > sWindow)
        NonEmptyVector.fromVector(segment.init) match {
          case Some(value) => sliceWithinSWindow(value)
          case _           => segment // This case shouldn't actually happen because of the first `if` condition
        }
      else segment
  }
}
