package co.topl.crypto.hash.digest

import cats.data.Validated
import co.topl.crypto.utils.Generators._
import org.scalatest.matchers.should.Matchers
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.ScalaCheckDrivenPropertyChecks

class Digest32Spec extends AnyPropSpec with ScalaCheckDrivenPropertyChecks with Matchers {

  property("should be invalid when length is less than digest size") {
    forAll(genByteArrayWithBoundedSize(0, Digest32.size - 1)) { bytes =>
      Digest32.validated(bytes).isValid shouldBe false
    }
  }

  property("should be invalid when length is greater than digest size") {
    val upperBound = 10000

    forAll(genByteArrayWithBoundedSize(Digest32.size + 1, upperBound)) { bytes =>
      Digest32.validated(bytes).isValid shouldBe false
    }
  }

  property("should be valid when length is equal to digest size and invalid otherwise") {
    forAll(genByteArrayOfSize(Digest32.size)) { bytes =>
      if (bytes.length == Digest32.size) Digest32.validated(bytes).isValid shouldBe true
      else Digest32.validated(bytes).isValid shouldBe false
    }
  }

  property("should contain correct bytes when length is digest size") {
    forAll(genByteArrayOfSize(Digest32.size)) { bytes =>
      Digest32.validated(bytes).getOrElse(throw new Exception("Invalid digest")).value shouldBe bytes
    }
  }
}
