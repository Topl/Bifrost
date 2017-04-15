package bifrost.serialization

import bifrost.BifrostGenerators
import examples.bifrost.contract.Agreement
import examples.bifrost.transaction._
import examples.bifrost.transaction.box.{BifrostBoxSerializer, ContractBox}
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import io.circe.Json, io.circe.parser._
import io.circe.syntax._

/**
  * Created by cykoz on 4/12/17.
  */
class SerializationTests extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators {

  property("StableCoinTransfer Serialization") {
    forAll(stableCoinTransferGen) {
      sc : StableCoinTransfer =>
        val parsed = TransferTransactionCompanion.parseBytes(
          TransferTransactionCompanion.toBytes(sc)
        ).get
        TransferTransactionCompanion.toBytes(parsed) shouldEqual TransferTransactionCompanion.toBytes(sc)
    }
  }

  property("ContractCreation Serialization") {
    forAll(contractCreationGen) {
      c: ContractCreation =>
        val parsed = ContractTransactionCompanion.parseBytes(
          ContractTransactionCompanion.toBytes(c)
        ).get
        ContractTransactionCompanion.toBytes(parsed) shouldEqual ContractTransactionCompanion.toBytes(c)
    }
  }

  property("Agreement Serialization") {
    forAll(agreementGen) {
      a: Agreement =>
        val parsed = AgreementCompanion.parseBytes(AgreementCompanion.toBytes(a)).get
        AgreementCompanion.toBytes(parsed) shouldEqual AgreementCompanion.toBytes(a)
    }
  }

  /* TODO Need a generator that generates erroneous JSON
  property("Agreement with no Nonce") {
    forAll(agreementGen) {
      a: Agreement =>
        val newTerms = parse("""{}""").getOrElse(Json.Null)
        a.terms.json = newTerms
        val parsed = AgreementCompanion.parseBytes(AgreementCompanion.toBytes(a)).get
        AgreementCompanion.toBytes(parsed) shouldEqual AgreementCompanion.toBytes(a)
    }
  }*/

  property("ContractBox Serialization") {
    forAll(bifrostBoxGen) {
      b: ContractBox =>
        val parsed = BifrostBoxSerializer.parseBytes(BifrostBoxSerializer.toBytes(b)).get
        val serialized = BifrostBoxSerializer.toBytes(parsed)
        serialized shouldEqual BifrostBoxSerializer.toBytes(b)
    }
  }
}
