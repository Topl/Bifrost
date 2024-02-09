package co.topl.algebras

import cats.{Applicative, Functor, Monad}
import cats.implicits._
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

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

    implicit def clockAsClockOps[F[_]](clock: ClockAlgebra[F]): ClockOps[F] =
      new ClockOps(clock)
  }

  object implicits extends Implicits
}

final class ClockOps[F[_]](val clock: ClockAlgebra[F]) extends AnyVal {

  def epochOf(slot: Slot)(implicit fApplicative: Applicative[F]): F[Epoch] =
    if (slot == 0L) (-1L).pure[F]
    else if (slot < 0L) (-2L).pure[F]
    else clock.slotsPerEpoch.map(numberOfSlots => (slot - 1) / numberOfSlots)

  def epochRange(epoch: Epoch)(implicit fApplicative: Applicative[F]): F[ClockAlgebra.SlotBoundary] =
    if (epoch == -1L) (0L to 0L).pure[F]
    else if (epoch < -1L) (-1L to -1L).pure[F]
    else clock.slotsPerEpoch.map(slotsPer => (epoch * slotsPer + 1) to (epoch + 1) * slotsPer)

  def isEpochStart(slot: Slot)(implicit fApplicative: Applicative[F]): F[Boolean] =
    if (slot == 0L) true.pure[F]
    else if (slot < -1L) true.pure[F]
    else clock.slotsPerEpoch.map(numberOfSlots => (slot - 1) % numberOfSlots === 0L)

  def operationalPeriodOf(slot: Slot)(implicit fFunctor: Functor[F]): F[Long] =
    clock.slotsPerOperationalPeriod.map((slot - 1) / _)

  def operationalPeriodRange(operationalPeriod: Long)(implicit fFunctor: Functor[F]): F[ClockAlgebra.SlotBoundary] =
    clock.slotsPerOperationalPeriod.map(slotsPer =>
      (operationalPeriod * slotsPer + 1) to (operationalPeriod + 1) * slotsPer
    )

  def globalOperationalPeriod(implicit fMonad: Monad[F]): F[Long] =
    clock.globalSlot.flatMap(operationalPeriodOf)
}
