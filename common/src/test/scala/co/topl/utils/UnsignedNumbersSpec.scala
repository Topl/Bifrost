package co.topl.utils

import co.topl.utils.UnsignedNumbers._
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class UnsignedNumbersSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  "UShort validated" should "be valid if number in range 0-65535" in {
    forAll(Gen.choose(0, 65535)) { value =>
      UShort.validated(value) shouldBe (Symbol("right"))
    }
  }

  "UShort validated" should "be invalid if number is outside of range 0-65535" in {
    forAll(Gen.oneOf(Gen.choose(Int.MinValue, -1), Gen.choose(65536, Int.MaxValue))) { value =>
      UShort.validated(value) shouldBe (Symbol("left"))
    }
  }

  "UInt validated" should "be valid if number in range 0-4294967295" in {
    forAll(Gen.choose(0L, 4294967295L)) { value =>
      UInt.validated(value) shouldBe (Symbol("right"))
    }
  }

  "UInt validated" should "be invalid if number is outside of range 0-4294967295" in {
    forAll(Gen.oneOf(Gen.choose(Long.MinValue, -1), Gen.choose(4294967296L, Long.MaxValue))) { value =>
      UInt.validated(value) shouldBe (Symbol("left"))
    }
  }
}
