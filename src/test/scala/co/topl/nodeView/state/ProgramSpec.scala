package co.topl.nodeView.state

import co.topl.{ BifrostGenerators, ValidGenerators }
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks }

class ProgramSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

//  def getPreExistingPolyBoxes(cc: ProgramCreation): Set[Box] = {
//    (cc
//      .preFeeBoxes
//      .flatMap {
//        case (prop, preBoxes) =>
//          preBoxes.map(b => PolyBox(prop, b._1, b._2))
//      } ++
//      cc
//        .preInvestmentBoxes
//        .map(b => PolyBox(cc.owner, b._1, b._2)))
//      .toSet
//  }
}
