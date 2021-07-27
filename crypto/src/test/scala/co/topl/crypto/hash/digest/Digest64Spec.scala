package co.topl.crypto.hash.digest

import cats.data.Validated
import co.topl.crypto.utils.Generators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Digest64Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("should be invalid when length is less than digest size") {
    forAll(genByteArrayWithBoundedSize(0, Digest64.size - 1)) { bytes =>
      Digest64.validated(bytes).isValid shouldBe false
    }
  }

  property("should be invalid when length is greater than digest size") {
    val upperBound = 10000

    forAll(genByteArrayWithBoundedSize(Digest64.size + 1, upperBound)) { bytes =>
      Digest64.validated(bytes).isValid shouldBe false
    }
  }

  property("should be valid when length is equal to digest size") {
    forAll(genByteArrayOfSize(Digest64.size)) { bytes =>
      if (bytes.length == Digest64.size) Digest64.validated(bytes).isValid shouldBe true
      else Digest64.validated(bytes).isValid shouldBe false
    }
  }

  property("should contain correct bytes when length is digest size") {
    forAll(genByteArrayOfSize(Digest64.size)) { bytes =>
      Digest64.validated(bytes).getOrElse(throw new Exception("Invalid digest")).value shouldBe bytes
    }
  }
}
