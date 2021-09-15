package co.topl.algebras

import cats.implicits._
import cats.{Apply, Functor, Semigroupal}
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

trait ClockAlgebra[F[_]] {
  def slotLength: F[FiniteDuration]
  def slotsPerEpoch: F[Long]
  def currentEpoch(): F[Epoch]
  def currentSlot(): F[Slot]
  def currentTimestamp(): F[Timestamp]
  def delayedUntilSlot(slot:           Slot): F[Unit]
  def delayedUntilTimestamp(timestamp: Timestamp): F[Unit]
}

object ClockAlgebra {

  trait Ops[F[_]] {
    val clock: ClockAlgebra[F]
    implicit def A: Apply[F]

    def epochOf(slot: Slot): F[Epoch] = clock.slotsPerEpoch.map(slot /)

    def epochBoundary(epoch: Epoch): F[NumericRange.Inclusive[Slot]] =
      clock.slotsPerEpoch.map(slotsPerEpoch => (epoch * slotsPerEpoch) to (((epoch + 1) * slotsPerEpoch) - 1))
  }

  trait AsOps {

    implicit def asClockOps[F[_]](c: ClockAlgebra[F])(implicit a: Apply[F]): Ops[F] = new Ops[F] {
      val clock: ClockAlgebra[F] = c

      implicit def A: Apply[F] = a
    }
  }

  object implicits extends AsOps
}
