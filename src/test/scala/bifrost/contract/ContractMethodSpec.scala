package bifrost.contract

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.JsonObject
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import org.graalvm.polyglot

class ContractMethodSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

  /*property("Can call a protocol level function from a contract") {
    forAll(contractGen) {
      c: Contract => {
        val party = propositionGen.sample.get

        //val result = Contract.execute(c, "createAsset")(party)()
      }
    }
  }*/
}