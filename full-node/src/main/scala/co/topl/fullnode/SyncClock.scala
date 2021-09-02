package co.topl.fullnode

import cats.Id
import co.topl.algebras.Clock
import co.topl.models.{Epoch, Slot, Timestamp}

import scala.concurrent.duration._

class SyncClock extends Clock[Id] {
  val slotLength: FiniteDuration = 10.millis

  val slotsPerEpoch: Long = 5000

  private val startTime = currentTimestamp()

  def currentEpoch(): Epoch = currentSlot() / slotsPerEpoch

  def currentSlot(): Slot = (currentTimestamp() - startTime) / slotLength.toMillis

  def currentTimestamp(): Timestamp = System.currentTimeMillis()

  def delayedUntilSlot(slot: Slot): Id[Unit] = {
    val durationMs = (slot - currentSlot()) * slotLength.toMillis
    if (durationMs > 0) Thread.sleep(durationMs)
  }

  def delayedUntilTimestamp(timestamp: Timestamp): Id[Unit] = {
    val durationMs = timestamp - currentTimestamp()
    if (durationMs > 0) Thread.sleep(durationMs)
  }
}
