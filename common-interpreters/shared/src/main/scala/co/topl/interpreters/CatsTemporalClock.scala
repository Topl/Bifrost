package co.topl.interpreters

import cats._
import cats.effect._
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.concurrent.duration._

object CatsTemporalClock {

  object Eval {

    def make[F[_]: Async](
      _slotLength:    FiniteDuration,
      _slotsPerEpoch: Long
    ): ClockAlgebra[F] =
      new ClockAlgebra[F] {
        private val startTime = System.currentTimeMillis()

        val slotLength: F[FiniteDuration] = _slotLength.pure[F]

        val slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

        val currentTimestamp: F[Timestamp] =
          Sync[F].delay(System.currentTimeMillis())

        val globalSlot: F[Slot] =
          currentTimestamp.map(currentTimestamp => (currentTimestamp - startTime) / _slotLength.toMillis)

        val currentEpoch: F[Epoch] =
          (globalSlot, slotsPerEpoch).mapN(_ / _)

        def delayedUntilSlot(slot: Slot): F[Unit] =
          globalSlot
            .map(currentSlot => (slot - currentSlot) * _slotLength)
            .flatMap(delay => if (delay.toMillis > 0) Temporal[F].sleep(delay) else Applicative[F].unit)

        def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] =
          currentTimestamp
            .map(currentTimestamp => (timestamp - currentTimestamp).millis)
            .flatMap(delay => if (delay.toMillis > 0) Temporal[F].sleep(delay) else Applicative[F].unit)
      }
  }
}
