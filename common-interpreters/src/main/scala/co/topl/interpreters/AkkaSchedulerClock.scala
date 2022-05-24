package co.topl.interpreters

import akka.actor.typed.ActorSystem
import cats.Monad
import cats.effect.Sync
import cats.effect.kernel.Async
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import java.time.Instant
import scala.concurrent.{Future, Promise}
import scala.concurrent.duration._

object AkkaSchedulerClock {

  object Eval {

    def make[F[_]: Monad: Async](
      _slotLength:     FiniteDuration,
      _slotsPerEpoch:  Long,
      genesisTime:     Instant
    )(implicit system: ActorSystem[_]): ClockAlgebra[F] =
      new ClockAlgebra[F] {
        import system.executionContext

        private val startTime = genesisTime.toEpochMilli

        val slotLength: F[FiniteDuration] = _slotLength.pure[F]

        val slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

        def currentTimestamp: F[Timestamp] = Sync[F].delay(System.currentTimeMillis())

        def globalSlot: F[Slot] =
          currentTimestamp.map(currentTimestamp => (currentTimestamp - startTime) / _slotLength.toMillis)

        def currentEpoch: F[Epoch] =
          (globalSlot, slotsPerEpoch).mapN(_ / _)

        // TODO: Deal with negative delay values
        // TODO: Don't repeat yourself
        def delayedUntilSlot(slot: Slot): F[(F[Unit], () => Unit)] =
          for {
            currentSlot <- globalSlot
            delay = (slot - currentSlot) * _slotLength
            promise = Promise[Unit]()
            callback = system.scheduler.scheduleOnce(delay, () => promise.success(()))
          } yield (Async[F].fromFuture(promise.future.pure[F]), callback)

        // TODO: Deal with negative delay values
        // TODO: Don't repeat yourself
        def delayedUntilTimestamp(timestamp: Timestamp): F[(F[Unit], () => Unit)] =
          for {
            now <- currentTimestamp
            delay = (timestamp - now).milli
            promise = Promise[Unit]()
            callback = system.scheduler.scheduleOnce(delay, () => promise.success(()))
          } yield (Async[F].fromFuture(promise.future.pure[F]), callback)
      }
  }
}
