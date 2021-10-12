package co.topl.consensus

import cats._
import cats.data._
import cats.implicits._
import co.topl.models.Slot
import co.topl.typeclasses._
import org.typelevel.log4cats.Logger

import scala.annotation.tailrec

object ChainSelection {

  private val standardOrder: Order[NonEmptyVector[SlotData]] =
    Order
      .whenEqual(
        Order.by[NonEmptyVector[SlotData], Int](_.length),
        Order.whenEqual(
          Order.by[NonEmptyVector[SlotData], Slot](_.last.slotId._1),
          Order.reverse(
            // TODO: Detect hash collisions?
            Order.whenEqual(
              Order.by[NonEmptyVector[SlotData], BigInt](h => BigInt(h.last.rho.data.toArray)),
              (_, _) => 1 // This may technically violate associativity expectations
            )
          )
        )
      )

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
                if (xSegment.head.slotId._1 >= ySegment.head.slotId._1) {
                  if (xSegment.head.slotId._1 === 0L) xSegment.pure[F] // Genesis case
                  else storage.get(xSegment.head.parentSlotId._2).map(_ +: xSegment)
                } else xSegment.pure[F]
              newYSegment <-
                if (ySegment.head.slotId._1 >= xSegment.head.slotId._1) {
                  if (ySegment.head.slotId._1 === 0L) ySegment.pure[F] // Genesis case
                  else storage.get(ySegment.head.parentSlotId._2).map(_ +: ySegment)
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
                s"Performing chain selection tineX.length=${xSegment.length} tineY.length=${ySegment.length} useDensityRule=$useDensityRule"
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

    @tailrec
    private def sliceWithinSWindow(segment: NonEmptyVector[SlotData]): NonEmptyVector[SlotData] =
      if (segment.length == 1) segment
      else if (segment.last.slotId._1 - segment.head.slotId._1 > sWindow)
        NonEmptyVector.fromVector(segment.init) match {
          case Some(value) => sliceWithinSWindow(value)
          case _           => segment // This case shouldn't actually happen because of the first `if` condition
        }
      else segment
  }
}
