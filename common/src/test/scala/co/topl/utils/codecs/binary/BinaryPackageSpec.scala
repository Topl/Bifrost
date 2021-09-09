package co.topl.utils.codecs.binary

import co.topl.utils.CommonGenerators
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}
import scodec.bits.BitVector

class BinaryPackageSpec
    extends AnyFlatSpec
    with CommonGenerators
    with ScalaCheckPropertyChecks
    with ScalaCheckDrivenPropertyChecks
    with Matchers {

  def stringWithByteLengthRange(from: Int, to: Int): Gen[String] =
    Gen.stringOfN(to, Gen.alphaChar).retryUntil { x =>
      val length = x.getBytes(stringCharacterSet).length

      length >= from && length <= to
    }

  "SmallString validated" should "be valid if String byte length is in range of 0-255" in {
    forAll(stringWithByteLengthRange(0, 255)) { value =>
      SmallString.validated(value) shouldBe Symbol("right")
    }
  }

  "SmallString validated" should "be invalid if String byte length is in range of 0-255" in {
    forAll(stringWithByteLengthRange(256, 100000)) { value =>
      SmallString.validated(value) shouldBe Symbol("left")
    }
  }

  "IntString validated" should "be valid if String byte length is greater than 255" in {
    forAll(stringWithByteLengthRange(256, 100000)) { value =>
      IntString.validated(value) shouldBe Symbol("right")
    }
  }
}
