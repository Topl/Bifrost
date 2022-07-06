package co.topl.crypto.generation.mnemonic

import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import co.topl.crypto.generation.mnemonic.MnemonicSizes.{`12`, `18`, `24`}
import co.topl.crypto.utils.Generators
import org.scalacheck.Gen
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class PhraseSpec extends AnyPropSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks {

  property("random entropy (of the correct length) should be a valid phrase") {
    forAll(Generators.mnemonicSizeGen) { mnemonicSize =>
      val entropy = Entropy.generate(mnemonicSize)
      val phrase = Phrase.fromEntropy(entropy, mnemonicSize, Language.English)

      phrase.isRight shouldBe true
      phrase.forall(_.value.length == mnemonicSize.wordLength) shouldBe true
    }
  }

  property("entropy should fail to create a phrase if there is a size mismatch") {
    Phrase
      .fromEntropy(
        Entropy.generate(MnemonicSizes.`24`),
        MnemonicSizes.`12`,
        Language.English
      )
      .isLeft shouldBe true
  }

  property("12 phrase mnemonic with valid words should be valid") {
    val phrase = "cat swing flag economy stadium alone churn speed unique patch report train"
    val mnemonic = Phrase.validated(phrase, Language.English)

    mnemonic.isRight shouldBe true
  }

  property("12 phrase mnemonic with invalid word length should be invalid") {
    val phrase = "result fresh margin life life filter vapor trim"
    val mnemonic = Phrase.validated(phrase, Language.English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with invalid words should be invalid") {
    val phrase = "amber glue hallway can truth drawer wave flex cousin grace close compose"
    val mnemonic = Phrase.validated(phrase, Language.English)

    mnemonic.isLeft shouldBe true
  }

  property("12 phrase mnemonic with valid words and invalid checksum should be invalid") {
    val phrase = "ugly wire busy skate slice kidney razor eager bicycle struggle aerobic picnic"
    val mnemonic = Phrase.validated(phrase, Language.English)

    mnemonic.isLeft shouldBe true
  }

  property("mnemonic with extra whitespace is valid") {
    val phrase = "vessel ladder alter error  federal sibling chat   ability sun glass valve picture"
    val mnemonic = Phrase.validated(phrase, Language.English)

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with extra whitespace has same value as single spaced") {
    val phrase1 = "vessel ladder alter error federal sibling chat ability sun glass valve picture"
    val test1 = Phrase.validated(phrase1, Language.English)

    val phrase2 = "vessel ladder alter error  federal sibling chat   ability sun glass valve picture"
    val test2 = Phrase.validated(phrase2, Language.English)

    test1.isRight shouldBe true
    test2.isRight shouldBe true
    test1 shouldBe test2
  }

  property("mnemonic with capital letters is valid") {
    val phrase = "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will"
    val mnemonic = Phrase.validated(phrase, Language.English)

    mnemonic.isRight shouldBe true
  }

  property("mnemonic with capital letters has same entropy as lowercase") {
    val phrase1 = "Legal Winner Thank Year Wave Sausage Worth Useful Legal " +
      "Winner Thank Year Wave Sausage Worth Useful Legal Will"
    val test1 = Phrase.validated(phrase1, Language.English)

    val phrase2 = "legal winner thank year wave sausage worth useful legal " +
      "winner thank year wave sausage worth useful legal will"
    val test2 = Phrase.validated(phrase2, Language.English)

    test1.isRight shouldBe true
    test2.isRight shouldBe true
    test1 shouldBe test2
  }

  property("mnemonic with unusual characters is invalid") {
    val entropy =
      Phrase.validated(
        "voi\uD83D\uDD25d come effort suffer camp su\uD83D\uDD25rvey warrior heavy shoot primary" +
        " clutch c\uD83D\uDD25rush" +
        " open amazing screen " +
        "patrol group space point ten exist slush inv\uD83D\uDD25olve unfold",
        Language.English
      )

    entropy.isLeft shouldBe true
  }

}
