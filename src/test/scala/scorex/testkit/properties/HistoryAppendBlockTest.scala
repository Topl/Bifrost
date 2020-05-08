package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.PersistentNodeViewModifier
import bifrost.consensus.{History, SyncInfo}
import bifrost.transaction.bifrostTransaction.Transaction
import bifrost.transaction.box.proposition.Proposition
import bifrost.utils.Logging
import scorex.testkit.TestkitHelpers

trait HistoryAppendBlockTest[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
SI <: SyncInfo,
HT <: History[P, TX, PM, SI, HT]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers with PropertyChecks
  with Logging with TestkitHelpers {
  val history: HT

  def genValidModifier(history: HT): PM

  property("Appended block is in history") {
    var h: HT = history
    check { _ =>
      val b = genValidModifier(h)
      h.modifierById(b.id).isDefined shouldBe false
      h = h.append(b).get._1
      h.modifierById(b.id).isDefined shouldBe true
    }
  }

}
