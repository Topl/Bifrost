package co.topl.algebras

import cats.Monad
import cats.implicits._
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration

/**
 * Provides global slot, epoch, and timing operations
 */
trait ClockAlgebra[F[_]] {
  def slotLength: F[FiniteDuration]
  // `R`
  def slotsPerEpoch: F[Long]
  def slotsPerOperationalPeriod: F[Long]
  def currentEpoch: F[Epoch]
  def globalSlot: F[Slot]
  def currentTimestamp: F[Timestamp]
  def forwardBiasedSlotWindow: F[Slot]
  def timestampToSlot(timestamp:       Timestamp): F[Slot]
  def slotToTimestamps(slot:           Slot): F[NumericRange.Inclusive[Timestamp]]
  def delayedUntilSlot(slot:           Slot): F[Unit]
  def delayedUntilTimestamp(timestamp: Timestamp): F[Unit]
}

object ClockAlgebra {

  type SlotBoundary = NumericRange.Inclusive[Slot]

  trait Implicits {

    implicit final class ClockOps[F[_]: Monad](clock: ClockAlgebra[F]) {

      def epochOf(slot: Slot): F[Epoch] =
        clock.slotsPerEpoch.map(numberOfSlots => slot / numberOfSlots).map(v => if (slot < 0) v - 1 else v)

      def epochRange(epoch: Epoch): F[SlotBoundary] =
        clock.slotsPerEpoch.map(slotsPer => (epoch * slotsPer) to (((epoch + 1) * slotsPer) - 1))

      def isEpochStart(slot: Slot): F[Boolean] =
        clock.slotsPerEpoch.map(numberOfSlots => slot % numberOfSlots === 0L)

      def operationalPeriodOf(slot: Slot): F[Long] =
        clock.slotsPerOperationalPeriod.map(slot / _)

      def operationalPeriodRange(operationalPeriod: Long): F[SlotBoundary] =
        clock.slotsPerOperationalPeriod.map(slotsPer =>
          (operationalPeriod * slotsPer) to (((operationalPeriod + 1) * slotsPer) - 1)
        )

      def globalOperationalPeriod: F[Long] =
        clock.globalSlot.flatMap(operationalPeriodOf)
    }
  }

  object implicits extends Implicits
}
