package co.topl.utils

import cats.Eq
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data, Latin1Data}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.must.Matchers.be
import org.scalatest.matchers.should.Matchers.{an, convertToAnyShouldWrapper}
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class StringDataTypesSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with ScalaCheckPropertyChecks {

  "Latin-1 Encoded String" should "be valid if only contains letters and numbers" in {
    forAll(Gen.alphaNumStr) { alphaNum =>
      Latin1Data.validated(alphaNum).isValid shouldBe true
    }
  }

  it should "be invalid if invalid Latin-1 characters" in {
    forAll(Gen.listOf(Gen.asciiChar)) { chars =>
      val inputStr = chars.mkString("")

      if (isValidLatin1(inputStr)) Latin1Data.validated(inputStr).isValid shouldBe true
      else Latin1Data.validated(inputStr).isValid shouldBe false
    }
  }

  it should "throw when calling unsafe with an invalid string" in {
    an[IllegalArgumentException] should be thrownBy Latin1Data.unsafe("ДОМ")
  }

  it should "be valid if valid Latin-1 characters" in {
    forAll(Gen.asciiStr) { str =>
      val latin1String = Latin1Data.validated(str)

      if (isValidLatin1(str)) latin1String.isValid shouldBe true
      else latin1String.isValid shouldBe false
    }
  }

  it should "return true when calling equals with same values" in {
    val str1 = Latin1Data.unsafe("hello")
    val str2 = Latin1Data.unsafe("hello")

    val areEqual = Eq[Latin1Data].eqv(str1, str2)

    areEqual shouldBe true
  }

  "Base-16 Encoded String" should "be valid when containing numbers and a-f and is even length" in {
    forAll(Gen.asciiStr) { str =>
      if (isValidBase16(str)) Base16Data.validated(str).isValid shouldBe true
      else Base16Data.validated(str).isValid shouldBe false
    }
  }

  it should "throw when calling unsafe with an invalid string" in {
    an[IllegalArgumentException] should be thrownBy Base16Data.unsafe("test")
  }

  it should "return true when calling equals with same values" in {
    val str1 = Base16Data.unsafe("1f")
    val str2 = Base16Data.unsafe("1f")

    val areEqual = Eq[Base16Data].eqv(str1, str2)

    areEqual shouldBe true
  }

  "Base-58 Encoded String" should "be valid when any alpha-numeric string except containing 0, O, I, and l" in {
    forAll(Gen.asciiStr) { asciiStr =>
      if (isValidBase58(asciiStr))
        Base58Data.validated(asciiStr).isValid shouldBe true
      else
        Base58Data.validated(asciiStr).isValid shouldBe false
    }
  }

  it should "throw when calling unsafe with an invalid string" in {
    an[IllegalArgumentException] should be thrownBy Base58Data.unsafe("hello")
  }

  it should "return true when calling equals with same values" in {
    val str1 = Base58Data.unsafe("zz")
    val str2 = Base58Data.unsafe("zz")

    val areEqual = Eq[Base58Data].eqv(str1, str2)

    areEqual shouldBe true
  }

  // https://www.ascii-code.com/
  private def isValidLatin1Char(c: Char): Boolean = (c >= 0 && c <= 127) || (c >= 159 && c <= 255)
  private def isValidLatin1(str: String): Boolean = str.forall(isValidLatin1Char)

  private def isValidBase16(str: String): Boolean =
    str.forall(x => (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F')) && str.length % 2 == 0

  private def isValidBase58(str: String): Boolean =
    str.forall(x =>
      (x >= '1' && x <= '9') ||
      (x >= 'A' && x <= 'Z' && x != 'I' && x != 'O') ||
      (x >= 'a' && x <= 'z' && x != 'l')
    )
}
