package scorex.testkit.properties

import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.PersistentNodeViewModifier
import bifrost.transaction.bifrostTransaction.Transaction
import bifrost.transaction.box.Box
import bifrost.transaction.box.proposition.Proposition
import bifrost.transaction.state.MinimalState
import scorex.testkit.{CoreGenerators, TestkitHelpers}

trait StateTests[P <: Proposition,
TX <: Transaction[P],
PM <: PersistentNodeViewModifier[P, TX],
B <: Box[P],
ST <: MinimalState[P, B, TX, PM, ST]] extends PropSpec with GeneratorDrivenPropertyChecks with Matchers
  with PropertyChecks with CoreGenerators {

  val state: ST

}
