package co.topl.utils

import co.topl.utils.StringTypes.{Base16String, Base58String, Latin1String, Utf8String}
import co.topl.utils.encode.{Base16, Base58}
import org.scalacheck.Gen
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class StringTypesSpec extends AnyFlatSpec with ScalaCheckDrivenPropertyChecks with ScalaCheckPropertyChecks {
  "Latin-1 Encoded String" should "be valid if only contains letters and numbers" in {
    forAll(Gen.alphaNumStr) { alphaNum =>
      Latin1String.validated(alphaNum).isValid shouldBe true
    }
  }

  it should "be invalid if invalid Latin-1 characters" in {
    forAll(Gen.listOf(Gen.asciiChar)) { chars =>
      val inputStr = chars.mkString("")

      if (isValidLatin1(inputStr)) Latin1String.validated(inputStr).isValid shouldBe true
      else Latin1String.validated(inputStr).isValid shouldBe false
    }
  }

  "UTF-8 Encoded String" should "be valid if only contains letters and numbers" in {
    forAll(Gen.alphaNumStr) { alphaNum =>
      Utf8String.validated(alphaNum).isValid shouldBe true
    }
  }

  "Base-16 Encoded String" should "be valid when containing numbers and a-f and is even length" in {
    forAll(Gen.asciiStr) { str =>
      if (isValidBase16(str)) Base16String.validated(str).isValid shouldBe true
      else Base16String.validated(str).isValid shouldBe false
    }
  }

  it should "decode to bytes without error when valid Base-58 string" in {
    forAll(Gen.alphaNumStr) { alphaNumStr =>
      Base16String.validated(alphaNumStr).map(Base16.decode)
    }
  }

  "Base-58 Encoded String" should "decode to bytes without error when valid Base-58 string" in {
    forAll(Gen.alphaNumStr) { alphaNumStr =>
      Base58String.validated(alphaNumStr).map(Base58.decode)
    }
  }

  it should "be valid when any alpha-numeric string except containing 0, O, I, and l" in {
    forAll(Gen.asciiStr) { asciiStr =>
      if (isValidBase58(asciiStr))
        Base58String.validated(asciiStr).isValid shouldBe true
      else
        Base58String.validated(asciiStr).isValid shouldBe false
    }
  }

  // https://www.ascii-code.com/
  private def isValidLatin1(c:   Char): Boolean = (c >= 0 && c <= 127) || (c >= 159 && c <= 255)
  private def isValidLatin1(str: String): Boolean = str.forall(isValidLatin1)

  private def isValidBase16(str: String): Boolean =
    str.forall(x => (x >= '0' && x <= '9') || (x >= 'a' && x <= 'f') || (x >= 'A' && x <= 'F')) && str.length % 2 == 0

  private def isValidBase58(str: String): Boolean =
    str.forall(x =>
      (x >= '1' && x <= '9') ||
      (x >= 'A' && x <= 'Z' && x != 'I' && x != 'O') ||
      (x >= 'a' && x <= 'z' && x != 'l')
    )
}
