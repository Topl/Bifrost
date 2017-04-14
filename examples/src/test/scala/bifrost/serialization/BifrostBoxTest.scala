package bifrost.serialization

import bifrost.BifrostGenerators
import examples.bifrost.contract.Agreement
import examples.bifrost.transaction.AgreementCompanion
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

  property("Agreement Serialization") {
    forAll(agreementGen) {
      b: Agreement =>
        val parsed = AgreementCompanion.parseBytes(AgreementCompanion.toBytes(b)).get
        AgreementCompanion.toBytes(parsed) shouldEqual AgreementCompanion.toBytes(b)
    }
  }

  property("ContractBox Serialization") {
    forAll(bifrostBoxGen) {
      b: ContractBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        // print(new String(serialized))
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }
}
