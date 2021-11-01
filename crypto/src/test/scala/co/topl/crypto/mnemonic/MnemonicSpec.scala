package co.topl.crypto.mnemonic

import co.topl.crypto.mnemonic.Language.English
import co.topl.crypto.mnemonic.MnemonicSize._
import co.topl.crypto.utils.Generators.genByteArrayOfSize
import co.topl.models.utility.Base58
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicSpec extends AnyPropSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks {

  implicit val entropyAsString: FromEntropy[String] =
    (e: Entropy) => Base58.encode(e.value)

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = FromEntropy.derive[String](phrase, Mnemonic12, English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"

    val mnemonic = FromEntropy.derive[String](phrase, Mnemonic12, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"

    val mnemonic = FromEntropy.derive[String](phrase, Mnemonic12, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = FromEntropy.derive[String](phrase, Mnemonic12, English)

    mnemonic.isLeft shouldBe true
  }

  def entropyLengthTest(bytes: Int, size: MnemonicSize): Unit =
    property(s"from entropy of length $bytes should be valid") {
      forAll(genByteArrayOfSize(bytes)) { entropy: Array[Byte] =>
        if (entropy.length == bytes) {
          val entropyString = FromEntropy.derive[String](entropy, size)

          entropyString.isRight shouldBe true
        }
      }
    }

  entropyLengthTest(16, Mnemonic12)
  entropyLengthTest(20, Mnemonic15)
  entropyLengthTest(24, Mnemonic18)
  entropyLengthTest(28, Mnemonic21)
  entropyLengthTest(32, Mnemonic24)

  property("mnemonic with extra whitespace is valid") {
    val mnemonic =
      FromEntropy.derive[String](
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        Mnemonic12,
        English
      )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same value as single spaced") {
    val expected =
      FromEntropy.derive[String](
        "vessel ladder alter error federal sibling chat ability sun glass valve picture",
        Mnemonic12,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    val result =
      FromEntropy.derive[String](
        "vessel ladder alter error  federal sibling chat   ability sun glass valve picture",
        Mnemonic12,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    result shouldBe expected
  }

  property("mnemonic with capital letters is valid") {
    val mnemonic = FromEntropy.derive[String](
      "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will",
      Mnemonic18,
      English
    )

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same entropy as lowercase") {
    val expectedEntropy =
      FromEntropy.derive[String](
        "legal winner thank year wave sausage worth useful legal " +
        "winner thank year wave sausage worth useful legal will",
        Mnemonic18,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    val result =
      FromEntropy.derive[String](
        "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
        "Winner Thank Year Wave Sausage Worth Useful Legal Will",
        Mnemonic18,
        English
      ) match {
        case Left(value)  => throw new Error("failed test")
        case Right(value) => value
      }

    result shouldBe expectedEntropy
  }

  property("mnemonic with unusual characters is invalid") {
    val entropy =
      FromEntropy.derive[String](
        "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
        " clutch c\uD83D\uDD25rush" +
        " open amazing screen " +
        "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
        Mnemonic24,
        English
      )

    entropy.isLeft shouldBe true
  }
}
