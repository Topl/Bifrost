package co.topl.consensus.interpreters

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class ConsensusDataEventSourcedStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  test("Retrieve the total active stake at a particular block") {}
  test("Return the active stake of an operator at a particular block") {}
  test("Return the registration of an operator at a particular block") {}
}
