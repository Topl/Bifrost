package bifrost.contract
import bifrost.contract.modules.BaseModuleWrapper
import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import org.scalacheck.Gen
import org.scalatest.{Matchers, PropSpec}
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}

import scala.util.{Failure, Random, Success, Try}


class ContractSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators{
/*
  property("Calling a method not in the contract will throw an error") {
    forAll(contractGen) {
      c: Contract => {
        forAll(stringGen.suchThat(!validContractMethods.contains(_))) {
          m: String => {
            val possibleArgs = JsonObject.empty

            val party = propositionGen.sample.get

            val result = Contract.execute(c, m)(party)(possibleArgs)
            assert(result.isFailure && result.failed.get.isInstanceOf[MatchError])
          }
        }
      }
    }
  }*/

  property("Json works properly for AgreementTerms") {
    forAll(validAgreementTermsGen) {
      t: AgreementTerms => {
        t.json.as[AgreementTerms].right.get shouldBe t
      }
    }
  }

  property("Cannot create AgreementTerms with too long of a string") {
    forAll(Gen.choose(16*1024 + 1, 100000)) {
      size: Int => {
        Try {
          AgreementTerms(Random.alphanumeric.take(size).mkString)
        } shouldBe a[Failure[_]]
      }
    }
  }

  property("Can create contract") {
    Try {
      Agreement(
        AgreementTerms("testing"),
        "myAssetCode",
        BaseModuleWrapper("test", validInitJsGen("test", "testCode").sample.get)(JsonObject.empty)
      )
    } shouldBe a[Success[_]]
  }

}