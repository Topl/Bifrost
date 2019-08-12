package bifrost.state

import bifrost.{BifrostGenerators, ValidGenerators}
import bifrost.transaction.bifrostTransaction.Role.Role
import bifrost.transaction.bifrostTransaction.ProgramCreation
import bifrost.transaction.box.BifrostBox
import bifrost.transaction.box.PolyBox
import bifrost.transaction.box.ProfileBox
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
        .map(b => PolyBox(cc.parties.head._1, b._1, b._2)))
      .toSet
  }

  def constructProfileBoxes(cc: ProgramCreation, roles: List[Role]): Set[ProfileBox] = {
    cc
      .parties
      .map {
        case (p: PublicKey25519Proposition, r: Role) => ProfileBox(p, positiveLongGen.sample.get, r.toString, "role")
      }.toSet ++
      (cc.signatures.keySet -- cc.parties.keySet)
        .zip((Stream continually roles).flatten)
        .map(t => ProfileBox(t._1, positiveLongGen.sample.get, t._2.toString, "role"))
  }
}
