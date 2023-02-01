package co.topl.algebras

import cats.Apply
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

  type EpochBoundary = NumericRange.Inclusive[Slot]

  trait Implicits {

    implicit final class ClockOps[F[_]: Apply](clock: ClockAlgebra[F]) {

      def epochOf(slot: Slot): F[Epoch] = clock.slotsPerEpoch.map(numberOfSlots => slot / numberOfSlots)

      def epochRange(epoch: Epoch): F[EpochBoundary] =
        clock.slotsPerEpoch.map(slotsPerEpoch => (epoch * slotsPerEpoch) to (((epoch + 1) * slotsPerEpoch) - 1))

      def epochBoundary(slot: Slot): F[Epoch] = clock.slotsPerEpoch.map(numberOfSlots => slot % numberOfSlots)
    }
  }

  object implicits extends Implicits
}
