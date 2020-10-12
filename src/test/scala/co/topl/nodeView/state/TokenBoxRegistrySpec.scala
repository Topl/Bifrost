package co.topl.nodeView.state

import co.topl.nodeView.NodeViewHolder.MS
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.OptionValues.convertOptionToValuable
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class TokenBoxRegistrySpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BeforeAndAfterAll
  with BifrostGenerators
  with ValidGenerators {

  val state: MS = State.readOrGenerate(settings)

  property("Token boxes should be inserted into the registry") {
    forAll(tokenBoxesGen) { tokens =>
      val keys = tokens.groupBy(_.proposition)
      directlyAddTBRStorage(scala.util.Random.nextInt(), tokens, state)
      keys.foreach { key =>
        val ids = key._2.map(_.id)
        state.registryLookup(key._1).value shouldEqual ids
      }
    }
  }

  override def afterAll() {
    state.closeStorage()
  }
}
