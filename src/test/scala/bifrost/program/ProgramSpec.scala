package bifrost.program

import java.time.Instant

import bifrost.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import org.scalacheck.Gen
import org.scalatest.prop.{GeneratorDrivenPropertyChecks, PropertyChecks}
import org.scalatest.{Matchers, PropSpec}
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import scorex.crypto.signatures.Curve25519

import scala.util.{Failure, Random, Success, Try}

class ProgramSpec extends PropSpec
  with PropertyChecks
  with GeneratorDrivenPropertyChecks
  with Matchers
  with BifrostGenerators
  with ValidGenerators {

    //TODO Replace with
    /*property("Calling a method not in the program will throw an error") {
      forAll(programGen) {
        c: Program => {
          forAll(stringGen.suchThat(!validProgramMethods.contains(_))) {
            m: String => {
              val possibleArgs = JsonObject.empty

              val party = propositionGen.sample.get

              val result = Program.execute(c, m)(party)(possibleArgs)
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
    forAll(Gen.choose(16 * 1024 + 1, 100000)) {
      size: Int => {
        Try {
          AgreementTerms(Random.alphanumeric.take(size).mkString)
        } shouldBe a[Failure[_]]
      }
    }
  }

  def mockAgreement: Json =
    ExecutionBuilder(
      AgreementTerms("testing"),
      "myAssetCode",
      ProgramPreprocessor(
        "test",
        validInitJsGen(
          "test",
          "testCode",
        ).sample.get
      )(JsonObject.empty)
    ).json

  def getMockPublicKeyProposition(fillByte: Byte): PublicKey25519Proposition = {
    PublicKey25519Proposition(Array.fill(Curve25519.KeyLength)(fillByte));
  }

  property("Can create program") {
    Try {
      Program(
        Map(getMockPublicKeyProposition(0) -> "hub", getMockPublicKeyProposition(1) -> "producer"),
        Instant.now.toEpochMilli,
        Array(),
        mockAgreement
      )
    } shouldBe a[Success[_]]
  }

  property("Can not create program due to incorrect number of parties") {
    Try {
      Program(
        Map(),
        Instant.now.toEpochMilli,
        Array(),
        mockAgreement
      )
    } shouldBe a[Failure[_]]
  }

}