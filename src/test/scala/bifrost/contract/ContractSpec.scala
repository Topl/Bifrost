package bifrost.contract

import java.time.Instant

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random, Success, Try}

class ContractSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

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
    }

  property("Json works properly for AgreementTerms") {
    forAll(validAgreementTermsGen) {
      t: AgreementTerms => {
        t.json.as[AgreementTerms].right.get shouldBe t
      }
    }
  }

  property("Cannot create AgreementTerms with too long of a string") {
    forAll(Gen.choose(16 * 1024 + 1, 100000)) {
      size: Int => {
        Try {
          AgreementTerms(Random.alphanumeric.take(size).mkString)
        } shouldBe a[Failure[_]]
      }
    }
  }

  def mockAgreement: Json =
    Agreement(
      AgreementTerms("testing"),
      "myAssetCode",
      BaseModuleWrapper(
        "test",
        validInitJsGen(
          "test",
          "testCode",
          Instant.now.toEpochMilli,
          Instant.now.toEpochMilli + 10000
        ).sample.get
      )(JsonObject.empty)
    ).json

  def getMockPublicKeyProposition(fillByte: Byte): PublicKey25519Proposition = {
    PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(fillByte));
  }

  property("Can create contract") {
    Try {
      Contract(
        Map(getMockPublicKeyProposition(0) -> "hub", getMockPublicKeyProposition(1) -> "producer"),
        Instant.now.toEpochMilli,
        Array(),
        mockAgreement
      )
    } shouldBe a[Success[_]]
  }

  property("Can not create contract due to incorrect number of parties") {
    Try {
      Contract(
        Map(),
        Instant.now.toEpochMilli,
        Array(),
        mockAgreement
      )
    } shouldBe a[Failure[_]]
  }

}