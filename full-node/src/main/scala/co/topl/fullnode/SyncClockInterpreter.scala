package co.topl.fullnode

import cats.Applicative
import cats.implicits._
import co.topl.algebras.ClockAlgebra
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.concurrent.duration._

class SyncClockInterpreter[F[_]: Applicative](_slotLength: FiniteDuration = 10.millis, _slotsPerEpoch: Long = 600)
    extends ClockAlgebra[F] {

  private val startTime = System.currentTimeMillis()

  def slotLength: F[FiniteDuration] = _slotLength.pure[F]

  def slotsPerEpoch: F[Epoch] = _slotsPerEpoch.pure[F]

  def currentEpoch(): F[Epoch] =
    (currentSlot(), slotsPerEpoch).mapN(_ / _)

  def currentSlot(): F[Slot] =
    currentTimestamp().map(currentTimestamp => (currentTimestamp - startTime) / _slotLength.toMillis)

  def currentTimestamp(): F[Timestamp] = System.currentTimeMillis().pure[F]

  def delayedUntilSlot(slot: Slot): F[Unit] =
    currentSlot()
      .map(currentSlot => (slot - currentSlot) * _slotLength.toMillis)
      .map(delay => if (delay > 0) Thread.sleep(delay))

  def delayedUntilTimestamp(timestamp: Timestamp): F[Unit] =
    currentTimestamp().map(timestamp - _).map(Thread.sleep)
}
