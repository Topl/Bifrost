package co.topl.interpreters

import cats.effect.Sync
import cats.effect.kernel.Async
import cats.implicits._
import cats.{Applicative, Monad}
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import java.time.Instant
import scala.collection.immutable.NumericRange
import scala.concurrent.duration._

object SchedulerClock {

  object Eval {

    def make[F[_]: Async](
      _slotLength:              FiniteDuration,
      _slotsPerEpoch:           Long,
      genesisTime:              Instant,
      _forwardBiasedSlotWindow: Slot
    ): ClockAlgebra[F] =
      new ClockAlgebra[F] {
        private val startTime = genesisTime.toEpochMilli

        override val slotLength: F[FiniteDuration] = _slotLength.pure[F]

        override val slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

        override def currentTimestamp: F[Timestamp] = Sync[F].delay(System.currentTimeMillis())

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
  }
}
