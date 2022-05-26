package co.topl.interpreters

import cats.effect.Sync
import cats.effect.kernel.Async
import cats.implicits._
import cats.{Applicative, Monad}
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import java.time.Instant
import scala.concurrent.duration._

object SchedulerClock {

  object Eval {

    def make[F[_]: Monad: Async](
      _slotLength:    FiniteDuration,
      _slotsPerEpoch: Long,
      genesisTime:    Instant
    ): ClockAlgebra[F] =
      new ClockAlgebra[F] {
        private val startTime = genesisTime.toEpochMilli

        val slotLength: F[FiniteDuration] = _slotLength.pure[F]

        val slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

        def currentTimestamp: F[Timestamp] = Sync[F].delay(System.currentTimeMillis())

        def globalSlot: F[Slot] =
          currentTimestamp.map(currentTimestamp => (currentTimestamp - startTime) / _slotLength.toMillis)

        def currentEpoch: F[Epoch] =
          (globalSlot, slotsPerEpoch).mapN(_ / _)

        def delayedUntilSlot(slot: Slot): F[Unit] =
          globalSlot.map(currentSlot => (slot - currentSlot) * _slotLength).flatMap(delayedFor)

        def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] =
          currentTimestamp.map(now => (timestamp - now).milli).flatMap(delayedFor)

        private def delayedFor(duration: FiniteDuration): F[Unit] =
          if (duration < Duration.Zero) Applicative[F].unit
          else Async[F].sleep(duration)
      }
  }
}
