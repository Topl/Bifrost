package bifrost.state

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.modifier.transaction.bifrostTransaction.ProgramCreation
import bifrost.modifier.box.Box
import bifrost.modifier.box.PolyBox
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckPropertyChecks, ScalaCheckDrivenPropertyChecks}

class ProgramSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  def getPreExistingPolyBoxes(cc: ProgramCreation): Set[Box] = {
    (cc
      .preFeeBoxes
      .flatMap {
        case (prop, preBoxes) =>
          preBoxes.map(b => PolyBox(prop, b._1, b._2))
      } ++
      cc
        .preInvestmentBoxes
        .map(b => PolyBox(cc.owner, b._1, b._2)))
      .toSet
  }
}

