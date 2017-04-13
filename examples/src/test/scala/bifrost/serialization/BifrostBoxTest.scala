package bifrost.serialization

import bifrost.BifrostGenerators
import examples.bifrost.transaction.{BifrostBox, BifrostBoxSerializer}
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

  property("BifrostBox[String] Serialization") {
    val bifrostBoxSerializer = new BifrostBoxSerializer[String]()
    forAll(bifrostBoxGen) {
      b: BifrostBox[String] =>
        val parsed = bifrostBoxSerializer.parseBytes(bifrostBoxSerializer.toBytes(b)).get
        bifrostBoxSerializer.toBytes(parsed) shouldEqual bifrostBoxSerializer.toBytes(b)
    }
  }
}
