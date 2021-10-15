package co.topl.consensus

import cats._
import cats.data._
import cats.implicits._
import co.topl.models._
import co.topl.typeclasses._
import co.topl.typeclasses.implicits._
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

object ChainSelection {

  private val standardOrder: Order[NonEmptyVector[SlotData]] = {
    val lengthOrder = Order.by[NonEmptyVector[SlotData], Int](_.length)
    val slotOrder = Order.by[NonEmptyVector[SlotData], Slot](-_.last.slotId.slot)
    // TODO: Implement proper sorting for `rho` (Byte Array)
    val rhoOrder = Order.reverse(Order.by[NonEmptyVector[SlotData], Byte](h => h.last.rho.data.toArray.head))
    Order
      .whenEqual(
        lengthOrder,
        Order.whenEqual(
          slotOrder,
          rhoOrder
        )
      )
  }

  /**
   * Between two tines, determines which one is "better"
   * @param kLookback The number of blocks in the backward-moving window of blocks for longest-chain rule
   * @param sWindow The number of slots of the forward-moving window of blocks for chain-density rule
   */
  def orderT[F[_]: Monad: Logger](
    storage:   SlotDataCache[F],
    kLookback: Long,
    sWindow:   Int
  ): OrderT[F, SlotData] = new ChainSelectionOrderT[F](storage, kLookback, sWindow)

  private class ChainSelectionOrderT[F[_]: Monad: Logger](
    storage:   SlotDataCache[F],
    kLookback: Long,
    sWindow:   Int
  ) extends OrderT[F, SlotData] {

    override def compare(x: SlotData, y: SlotData): F[Int] =
      if (x === y) 0.pure[F]
      else
        TineComparisonTraversal(
          NonEmptyVector.one(x),
          NonEmptyVector.one(y),
          useDensityRule = false
        )
          .iterateWhileM(_.next)(!_.sharesCommonAncestor)
          .flatTap {
            case TineComparisonTraversal(xSegment, ySegment, useDensityRule)
                if xSegment.length > 1 && ySegment.length > 1 =>
              Logger[F].info(
                "Performing chain selection" +
                show" tineX=[${xSegment.length}](${xSegment.head.slotId}..${xSegment.last.slotId})" +
                show" tineY=[${ySegment.length}](${ySegment.head.slotId}..${ySegment.last.slotId})" +
                show" useDensityRule=$useDensityRule"
              )
            case _ =>
              Applicative[F].unit
          }
          .map(_.comparisonResult)

    // TODO: Best data structure for: prepend, length, and "dropRightWhile"?
    private case class TineComparisonTraversal(
      xSegment:       NonEmptyVector[SlotData],
      ySegment:       NonEmptyVector[SlotData],
      useDensityRule: Boolean
    ) {

      def next: F[TineComparisonTraversal] = {
        def buildNextSegment(segment: NonEmptyVector[SlotData], other: NonEmptyVector[SlotData]) =
          if (segment.head.slotId.slot >= other.head.slotId.slot) {
            if (segment.head.slotId.slot === 0L) segment.pure[F] // Genesis case
            else storage.get(segment.head.parentSlotId.blockId).map(_ +: segment)
          } else segment.pure[F]
        // Prepend/accumulate the next parent header for the xSegment and the ySegment
        for {
          newXSegment <- buildNextSegment(xSegment, ySegment)
          newYSegment <- buildNextSegment(ySegment, xSegment)
          newUseChainDensity = // Once we've traversed back K number of blocks, switch to the chain density rule
            useDensityRule || (newXSegment.length > kLookback) || (newYSegment.length > kLookback)
        } yield TineComparisonTraversal(
          if (newUseChainDensity) sliceWithinSWindow(newXSegment) else newXSegment,
          if (newUseChainDensity) sliceWithinSWindow(newYSegment) else newYSegment,
          newUseChainDensity
        )
      }

      def sharesCommonAncestor: Boolean =
        xSegment.head === ySegment.head

      def comparisonResult: Int =
        if (useDensityRule)
          // Density comparison
          xSegment.length.compare(ySegment.length)
        else
          standardOrder.compare(xSegment, ySegment)

      /**
       * In cases where a common-ancestor search traces back more than `kLookback` blocks, we only need to aggregate
       * blocks within the forward-traversing sWindow.  The result is used when calculating chain density.
       */
      @tailrec
      private def sliceWithinSWindow(segment: NonEmptyVector[SlotData]): NonEmptyVector[SlotData] =
        if (segment.length === 1) segment
        else if (segment.last.slotId.slot - segment.head.slotId.slot > sWindow)
          NonEmptyVector.fromVector(segment.init) match {
            case Some(value) => sliceWithinSWindow(value)
            case _           => segment // This case shouldn't actually happen because of the first `if` condition
          }
        else segment
    }
  }
}
