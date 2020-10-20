package co.topl.nodeView.state

import co.topl.crypto.FastCryptographicHash
import co.topl.modifier.ModifierId
import co.topl.nodeView.NodeViewHolder.MS
import co.topl.{BifrostGenerators, ValidGenerators}
import com.google.common.primitives.Ints
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
      directlyAddTBRStorage(scala.util.Random.nextInt, tokens, state)
      keys.foreach { key =>
        val ids = key._2.map(_.id)
        state.registryLookup(key._1).value shouldEqual ids
      }
    }
  }

  property("Rolling back should remove tokens from registry") {
    forAll(tokenBoxesGen) { tokens =>
      val tbr = state.tbrOpt.get
      val version = ModifierId(FastCryptographicHash(Ints.toByteArray(scala.util.Random.nextInt)))
      val keys = tokens.groupBy(_.proposition)
      val update = keys.map(k => k._1 -> k._2.map(_.nonce))

      val newTbr = tbr.update(version, Map(), update).get
      val rollback = newTbr.rollbackTo(state.version).get

      keys.foreach { key =>
        val ids = key._2.map(_.id)
        state.registryLookup(key._1) shouldBe None
      }
    }
  }

  override def afterAll() {
    state.closeStorage()
  }
}
