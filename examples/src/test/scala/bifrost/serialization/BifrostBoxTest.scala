package bifrost.serialization

import bifrost.BifrostGenerators
import examples.bifrost.transaction.box.{BifrostBoxSerializer, ContractBox}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

/**
  * Created by cykoz on 4/12/17.
  */
class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {

  property("BifrostBox Serialization") {
    forAll(bifrostBoxGen) {
      b: ContractBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        BifrostBoxSerializer.toBytes(parsed) shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }
}
