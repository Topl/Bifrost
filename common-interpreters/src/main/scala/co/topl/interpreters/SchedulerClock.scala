package co.topl.interpreters

import cats.effect.Sync
import cats.effect.kernel.Async
import cats.implicits._
import cats.Applicative
import cats.effect.Resource
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import java.time.Instant
import scala.collection.immutable.NumericRange
import scala.concurrent.duration._

object SchedulerClock {

  /**
   * Constructs a Clock interpretation that combines the local system clock with a variable time skew.  Slot-based
   * interpretations combine the skewed clock with the given configurations.
   * @param _slotLength The duration of a single slot
   * @param _slotsPerEpoch The number of slots in a single epoch
   * @param genesisTime The timestamp of the big-bang block
   * @param _forwardBiasedSlotWindow The allowable number of slots in the future for which we can accept remote peer data (like a block header)
   * @param timeSkew A lookup function which returns the number of milliseconds by which the local time should be adjusted
   */
  def make[F[_]: Async](
    _slotLength:              FiniteDuration,
    _slotsPerEpoch:           Long,
    genesisTime:              Instant,
    _forwardBiasedSlotWindow: Slot,
    timeSkew:                 () => F[Long]
  ): Resource[F, ClockAlgebra[F]] =
    Resource.pure(
      new ClockAlgebra[F] {
        private val startTime = genesisTime.toEpochMilli

        override val slotLength: F[FiniteDuration] = _slotLength.pure[F]

        override val slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

        override def currentTimestamp: F[Timestamp] =
          (Sync[F].delay(System.currentTimeMillis()), timeSkew()).mapN(_ + _)

        override def timestampToSlot(timestamp: Timestamp): F[Slot] =
          ((timestamp - startTime) / _slotLength.toMillis).pure[F]

        override def slotToTimestamps(slot: Slot): F[NumericRange.Inclusive[Long]] =
          Sync[F].delay {
            val startTimestamp = startTime + (slot * _slotLength.toMillis)
            val endTimestamp = startTimestamp + (_slotLength.toMillis - 1)
            NumericRange.inclusive(startTimestamp, endTimestamp, 1L)
          }

        override val forwardBiasedSlotWindow: F[Slot] = _forwardBiasedSlotWindow.pure[F]

        override def globalSlot: F[Slot] = currentTimestamp.flatMap(timestampToSlot)

        override def currentEpoch: F[Epoch] =
          (globalSlot, slotsPerEpoch).mapN(_ / _)

        override def delayedUntilSlot(slot: Slot): F[Unit] =
          globalSlot.map(currentSlot => (slot - currentSlot) * _slotLength).flatMap(delayedFor)

        override def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] =
          currentTimestamp.map(now => (timestamp - now).milli).flatMap(delayedFor)

        private def delayedFor(duration: FiniteDuration): F[Unit] =
          if (duration < Duration.Zero) Applicative[F].unit
          else Async[F].sleep(duration)
      }
    )
}
