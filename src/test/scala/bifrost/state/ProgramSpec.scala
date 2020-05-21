package bifrost.state

import bifrost.modifier.box.{BifrostBox, PolyBox}
import bifrost.modifier.transaction.bifrostTransaction.ProgramCreation
import bifrost.{BifrostGenerators, ValidGenerators}
import org.scalatest.{Matchers, PropSpec}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class ProgramSpec extends PropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  def getPreExistingPolyBoxes(cc: ProgramCreation): Set[BifrostBox] = {
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

