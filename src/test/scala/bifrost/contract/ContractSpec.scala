package bifrost.contract


import bifrost.{BifrostGenerators, ValidGenerators}
<<<<<<< HEAD:src/test/scala/bifrost/contract/ContractSpec.scala
=======
import examples.bifrost.contract.Contract
import io.circe.{Json, JsonObject}
>>>>>>> Merged dev into local:examples/src/test/scala/bifrost/contract/ContractSpec.scala
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import scorex.crypto.encode.Base58
import io.circe.syntax._

import scala.util.Success

class ContractSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators{

  property("Calling a method in the contract will with proper params not throw an error") {
    forAll(contractGen) {
      c: Contract => {
        validContractMethods.foreach(m => {
          val possibleArgs = Map(
            "actor" -> Base58.encode(c.Producer.pubKeyBytes).asJson,
            "producer" -> Base58.encode(c.Producer.pubKeyBytes).asJson,
            "hub" -> Base58.encode(c.Hub.pubKeyBytes).asJson
          ).asJson

          val result = Contract.execute(c, m)(possibleArgs.asObject.get)
          println(s"Calling $m resulted in $result")
          assert(result.isSuccess)
        })
      }
    }
  }

  property("Calling a method not in the contract will throw an error") {

  }

  property("Calling methods with valid Proposition should result in Success") {

  }

  property("Calling a method with invalid Proposition (e.g. Producer for Investor) results in Failure") {

  }

  property("Contract created from Json should have fields that match") {

  }

  property("Method executed that updates Contract should return updated Contract object") {

  }
}