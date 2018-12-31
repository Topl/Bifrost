package scorex.testkit.properties

import org.scalacheck.Gen
import bifrost.PersistentNodeViewModifier
import bifrost.transaction.Transaction
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.state.{MinimalState, StateChanges}

import scala.util.Random

trait StateApplyChangesTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: MinimalState[P, B, TX, PM, ST]] extends StateTests[P, TX, PM, B, ST] {

  val stateChangesGenerator: Gen[StateChanges[P, B]]

  property("State should be able to add a box") {
    forAll(stateChangesGenerator) { c =>
      val newState = state.applyChanges(c, Array.fill(32)(Random.nextInt(Byte.MaxValue).toByte)).get
      c.toAppend.foreach { b =>
        newState.closedBox(b.id).isDefined shouldBe true
      }
    }
  }


}
