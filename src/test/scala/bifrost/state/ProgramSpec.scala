package bifrost.state

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.transaction.bifrostTransaction.ProgramCreation
import bifrost.transaction.box.BifrostBox
import bifrost.transaction.box.PolyBox
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import bifrost.transaction.box.proposition.PublicKey25519Proposition

class ProgramSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
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

