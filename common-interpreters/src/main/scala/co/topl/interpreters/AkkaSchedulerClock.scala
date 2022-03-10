package co.topl.interpreters

import akka.actor.typed.ActorSystem
import cats.Monad
import cats.effect.Sync
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.concurrent.Future
import scala.concurrent.duration._

object AkkaSchedulerClock {

  object Eval {

    def make[F[_]: Monad: Async](
      _slotLength:     FiniteDuration,
      _slotsPerEpoch:  Long
    )(implicit system: ActorSystem[_]): ClockAlgebra[F] =
      new ClockAlgebra[F] {

        private val startTime = System.currentTimeMillis()

        val slotLength: F[FiniteDuration] = _slotLength.pure[F]

        val slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

        def currentTimestamp: F[Timestamp] = Sync[F].delay(System.currentTimeMillis())

        def globalSlot: F[Slot] =
          currentTimestamp.map(currentTimestamp => (currentTimestamp - startTime) / _slotLength.toMillis)

        def currentEpoch: F[Epoch] =
          (globalSlot, slotsPerEpoch).mapN(_ / _)

        def delayedUntilSlot(slot: Slot): F[Unit] =
          Async[F].fromFuture(
            globalSlot
              .map(currentSlot => (slot - currentSlot) * _slotLength)
              .map(delay => if (delay.toMillis > 0) akka.pattern.after(delay)(Future.unit) else Future.unit)
          )

        def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] =
          Async[F].fromFuture(
            currentTimestamp
              .map(currentTimestamp => akka.pattern.after((timestamp - currentTimestamp).millis)(Future.unit))
          )
      }
  }
}
