package co.topl.program

import java.time.Instant

import co.topl.nodeView.state.box.proposition.PublicKey25519Proposition
import co.topl.{BifrostGenerators, ValidGenerators}
import io.circe.{Json, JsonObject}
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scorex.crypto.signatures.{Curve25519, PublicKey}

import scala.util.{Failure, Random, Success, Try}

class ProgramSpec extends AnyPropSpec
  with ScalaCheckPropertyChecks
  with ScalaCheckDrivenPropertyChecks
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

  property("Json works properly for ExecutionBuilderTerms") {
    forAll(validExecutionBuilderTermsGen) {
      t: ExecutionBuilderTerms => {
        t.json.as[ExecutionBuilderTerms].right.get shouldBe t
      }
    }
  }

  property("Cannot create ExecutionBuilderTerms with too long of a string") {
    forAll(Gen.choose(16 * 1024 + 1, 100000)) {
      size: Int => {
        Try {
          ExecutionBuilderTerms(Random.alphanumeric.take(size).mkString)
        } shouldBe a[Failure[_]]
      }
    }
  }

  def mockExecutionBuilder: Json =
    ExecutionBuilder(
      ExecutionBuilderTerms("testing"),
      "myAssetCode",
      ProgramPreprocessor(
        "test",
        validInitJsGen.sample.get
      )(JsonObject.empty)
    ).json

  def getMockPublicKeyProposition(fillByte: Byte): PublicKey25519Proposition = {
    PublicKey25519Proposition(PublicKey @@ Array.fill(Curve25519.KeyLength)(fillByte))
  }

  property("Can create program") {
    Try {
      Program(
        Map(getMockPublicKeyProposition(0) -> "hub", getMockPublicKeyProposition(1) -> "producer"),
        Instant.now.toEpochMilli,
        Array(),
        mockExecutionBuilder
      )
    } shouldBe a[Success[_]]
  }

  property("Can not create program due to incorrect number of parties") {
    Try {
      Program(
        Map(),
        Instant.now.toEpochMilli,
        Array(),
        mockExecutionBuilder
      )
    } shouldBe a[Failure[_]]
  }
}