package co.topl.algebras

import co.topl.models.{Epoch, Slot, Timestamp}

import scala.concurrent.duration.FiniteDuration

trait Clock[F[_]] {
  def slotLength: FiniteDuration
  def slotsPerEpoch: Long
  def currentEpoch(): Epoch
  def currentSlot(): Slot
  def currentTimestamp(): Timestamp
  def delayedUntilSlot(slot:           Slot): F[Unit]
  def delayedUntilTimestamp(timestamp: Timestamp): F[Unit]
}
