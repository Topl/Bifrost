package co.topl.crypto.hash

import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks
import org.scalatest.matchers.should.Matchers
import cats.data.Validated
import co.topl.crypto.utils.Generators._

class Digest64Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers with DigestSpec {

  testsForDigest[Digest64]("Digest64")

  property("can be instantiated with array of any size") {
    forAll(genRandomlySizedByteArray) { bytes =>
      val digest = Digest64(bytes)

      digest.value.length shouldBe bytes.length
    }
  }

  property("when validated should be invalid when length is less than digest size") {
    forAll(genByteArrayWithBoundedSize(0, Digest64.size - 1)) { bytes =>
      Digest64.validated(bytes) shouldBe Validated.Invalid(IncorrectSize)
    }
  }

  property("when validated should be invalid when length is greater than digest size") {
    val upperBound = 10000

    forAll(genByteArrayWithBoundedSize(Digest64.size + 1, upperBound)) { bytes =>
      Digest64.validated(bytes) shouldBe Validated.Invalid(IncorrectSize)
    }
  }

  property("when validated should be valid when length is equal to digest size") {
    forAll(genByteArrayOfSize(Digest64.size)) { bytes =>
      Digest64.validated(bytes) shouldBe Validated.Valid(Digest32(bytes))
    }
  }

  property("when validated should contain correct bytes when length is digest size") {
    forAll(genByteArrayOfSize(Digest64.size)) { bytes =>
      Digest64.validated(bytes).valueOr(_ => Digest64(Array[Byte]())).value shouldBe bytes
    }
  }

}
