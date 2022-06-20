package co.topl.crypto.generation

import co.topl.crypto.generation.mnemonic.Language.English
import co.topl.crypto.generation.mnemonic.MnemonicSizes._
import co.topl.crypto.generation.mnemonic.{Entropy, MnemonicSize}
import co.topl.crypto.utils.Generators.genByteArrayOfSize
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class MnemonicSpec extends AnyPropSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks {

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = Entropy.fromMnemonicString(phrase, `12`, English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"
    val mnemonic = Entropy.fromMnemonicString(phrase, `12`, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"
    val mnemonic = Entropy.fromMnemonicString(phrase, `12`, English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = Entropy.fromMnemonicString(phrase, `12`, English)

    mnemonic.isLeft shouldBe true
  }

  def entropyLengthTest(bytes: Int, size: MnemonicSize): Unit =
    property(s"from entropy of length $bytes should be valid") {
      forAll(genByteArrayOfSize(bytes)) { testBytes: Array[Byte] =>
        if (testBytes.length == bytes) {
          val entropyString = Entropy.fromBytes(testBytes, size)

          entropyString.isRight shouldBe true
        }
      }
    }

  entropyLengthTest(16, `12`)
  entropyLengthTest(20, `15`)
  entropyLengthTest(24, `18`)
  entropyLengthTest(28, `21`)
  entropyLengthTest(32, `24`)

  property("mnemonic with extra whitespace is valid") {
    val phrase = "vessel ladder alter error  federal sibling chat   ability sun glass valve picture"
    val mnemonic = Entropy.fromMnemonicString(phrase, `12`, English)

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same value as single spaced") {
    val phrase1 = "vessel ladder alter error federal sibling chat ability sun glass valve picture"
    val test1 = Entropy.fromMnemonicString(phrase1, `12`, English)

    val phrase2 = "vessel ladder alter error  federal sibling chat   ability sun glass valve picture"
    val test2 = Entropy.fromMnemonicString(phrase2, `12`, English)

    test1.isRight shouldBe true
    test2.isRight shouldBe true
    test1 shouldBe test2
  }

  property("mnemonic with capital letters is valid") {
    val phrase = "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will"
    val mnemonic = Entropy.fromMnemonicString(phrase, `18`, English)

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same entropy as lowercase") {
    val phrase1 = "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will"
    val test1 = Entropy.fromMnemonicString(phrase1, `18`, English)

    val phrase2 = "legal winner thank year wave sausage worth useful legal " +
      "winner thank year wave sausage worth useful legal will"
    val test2 = Entropy.fromMnemonicString(phrase2, `18`, English)

    test1.isRight shouldBe true
    test2.isRight shouldBe true
    test1 shouldBe test2
  }

  property("mnemonic with unusual characters is invalid") {
    val entropy =
      Entropy.fromMnemonicString(
        "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
        " clutch c\uD83D\uDD25rush" +
        " open amazing screen " +
        "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
        `24`,
        English
      )

    entropy.isLeft shouldBe true
  }
}
