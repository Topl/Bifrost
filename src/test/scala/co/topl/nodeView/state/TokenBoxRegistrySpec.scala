package co.topl.nodeView.state

import co.topl.nodeView.NodeViewHolder.MS
import co.topl.{BifrostGenerators, ValidGenerators}
import org.scalatest.BeforeAndAfterAll
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

import scala.reflect.io.Path
import scala.util.Try

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
      directlyAddTBRStorage(scala.util.Random.nextInt(), tokens, state)
      tokens.foreach(t => state.getBox(t.id) should not be None)
    }
  }

  property("Rollback should have worked and recreated above changes exactly") {


  }

  override def afterAll() {
    state.closeStorage()
  }
}
