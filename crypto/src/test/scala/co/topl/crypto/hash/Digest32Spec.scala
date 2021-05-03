package co.topl.crypto.hash

import cats.data.Validated
import co.topl.crypto.utils.Generators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Digest32Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with DigestSpec {

  testsForDigest[Digest32]("Digest32")

  property("can be instantiated with array of any size") {
    forAll(genRandomlySizedByteArray) { bytes =>
      val digest = Digest32(bytes)

      digest.value.length shouldBe bytes.length
    }
  }

  property("when validated should be invalid when length is less than digest size") {
    forAll(genByteArrayWithBoundedSize(0, Digest32.size - 1)) { bytes =>
      Digest32.validated(bytes) shouldBe Validated.Invalid(IncorrectSize)
    }
  }

  property("when validated should be invalid when length is greater than digest size") {
    val upperBound = 10000

    forAll(genByteArrayWithBoundedSize(Digest32.size + 1, upperBound)) { bytes =>
      Digest32.validated(bytes) shouldBe Validated.Invalid(IncorrectSize)
    }
  }

  property("when validated should be valid when length is equal to digest size") {
    forAll(genByteArrayOfSize(Digest32.size)) { bytes =>
      Digest32.validated(bytes) shouldBe Validated.Valid(Digest32(bytes))
    }
  }

  property("when validated should contain correct bytes when length is digest size") {
    forAll(genByteArrayOfSize(Digest32.size)) { bytes =>
      Digest32.validated(bytes).valueOr(_ => Digest32(Array[Byte]())).value shouldBe bytes
    }
  }
}
