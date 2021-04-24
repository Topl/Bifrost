package co.topl.program

import java.time.Instant

import co.topl.attestation.PublicKeyPropositionCurve25519
import io.circe.{Json, JsonObject}
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scorex.crypto.signatures.{Curve25519, PublicKey}

import scala.util.{Failure, Success, Try}

class ProgramSpec
    extends AnyPropSpec
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers
    with ProgramGenerators {

  property("Json works properly for ExecutionBuilderTerms") {
    forAll(validExecutionBuilderTermsGen) { t: ExecutionBuilderTerms =>
      val res = t.json.as[ExecutionBuilderTerms] match { case Right(re) => re; case Left(ex) => throw ex }
      res shouldBe t
    }
  }

  def mockExecutionBuilder: Json =
    ExecutionBuilder(
      ExecutionBuilderTerms("testing"),
      "myAssetCode",
      ProgramPreprocessor(
        "test",
        sampleUntilNonEmpty(validInitJsGen())
      )(JsonObject.empty)
    ).json

  def getMockPublicKeyProposition(fillByte: Byte): PublicKeyPropositionCurve25519 =
    PublicKeyPropositionCurve25519(PublicKey @@ Array.fill(Curve25519.KeyLength)(fillByte))

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

  property("ExecutionBuilder serialization") {
    forAll(validExecutionBuilderGen()) { a: ExecutionBuilder =>
      val parsed = ExecutionBuilderSerializer
        .parseBytes(ExecutionBuilderSerializer.toBytes(a))
        .get

      ExecutionBuilderSerializer.toBytes(parsed) sameElements ExecutionBuilderSerializer.toBytes(a) shouldBe true
    }
  }
}
