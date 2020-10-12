package co.topl.nodeView.state

import co.topl.nodeView.NodeViewHolder.MS
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.BeforeAndAfterAll
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
      val keys = tokens.groupBy(tk => tk.proposition)
      directlyAddTBRStorage(scala.util.Random.nextInt(), tokens, state)
      keys.foreach { key =>
        state.getTokenBoxes(key._1).get shouldEqual key._2
      }
    }
  }

  override def afterAll() {
    state.closeStorage()
  }
}
