package co.topl.algebras

import co.topl.models.{Epoch, Slot, Timestamp}

import scala.collection.immutable.NumericRange
import scala.concurrent.duration.FiniteDuration
import scala.language.implicitConversions

trait Clock[F[_]] {
  def slotLength: FiniteDuration
  def slotsPerEpoch: Long
  def currentEpoch(): Epoch
  def currentSlot(): Slot
  def currentTimestamp(): Timestamp
  def delayedUntilSlot(slot:           Slot): F[Unit]
  def delayedUntilTimestamp(timestamp: Timestamp): F[Unit]
}

object Clock {

  trait Ops[F[_]] {
    val clock: Clock[F]

    def epochOf(slot: Slot): Epoch = slot / clock.slotsPerEpoch

    def epochBoundary(epoch: Epoch): NumericRange.Inclusive[Slot] =
      (epoch * clock.slotsPerEpoch) to (((epoch + 1) * clock.slotsPerEpoch) - 1)
  }

  trait AsOps {

    implicit def asClockOps[F[_]](c: Clock[F]): Ops[F] = new Ops[F] {
      val clock: Clock[F] = c
    }
  }

  object implicits extends AsOps
}
