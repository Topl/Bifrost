package co.topl.consensus.interpreters

import munit.{CatsEffectSuite, ScalaCheckEffectSuite}
import org.scalamock.munit.AsyncMockFactory

class ConsensusValidationStateSpec extends CatsEffectSuite with ScalaCheckEffectSuite with AsyncMockFactory {
  test("Retrieve relative stake by address along epoch > 1 chains") {}
  test("Retrieve registration by address along epoch > 1 chains") {}
  test("Retrieve relative stake by address along epoch <= 1 chains using genesis data") {}
  test("Retrieve registration by address along epoch <= 1 chains using genesis data") {}
}
