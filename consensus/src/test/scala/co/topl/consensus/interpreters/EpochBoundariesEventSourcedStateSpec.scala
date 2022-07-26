package co.topl.consensus.interpreters

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalacheck.Gen
import org.scalacheck.effect.PropF
import org.scalamock.munit.AsyncMockFactory

import co.topl.models.ModelGenerators._

class EpochBoundariesEventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {

  test("Retrieve an epoch boundary block ID for a positive/existent epoch") {
    val slotData = Iterator.continually(arbitrarySlotData.arbitrary.first).zipWithIndex
  }
  test("Return None for a negative or non-existent epoch") {}
}
