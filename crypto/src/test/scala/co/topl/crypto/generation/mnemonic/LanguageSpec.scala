package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.generation.mnemonic.Language._
import co.topl.crypto.utils.Generators
import org.scalacheck.Gen
import org.scalatest.funspec.AnyFunSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import org.scalatest.propspec.AnyPropSpec
import org.scalatestplus.scalacheck.{ScalaCheckDrivenPropertyChecks, ScalaCheckPropertyChecks}

class LanguageSpec extends AnyPropSpec with ScalaCheckPropertyChecks with ScalaCheckDrivenPropertyChecks {

  val languages = Seq(
    English,
    ChineseSimplified,
    ChineseTraditional,
    Portuguese,
    Czech,
    Spanish,
    Italian,
    French,
    Japanese,
    Korean
  )

  languages.foreach { lang =>
    property(s"${lang.toString} should have a valid checksum with included word list") {
      LanguageWordList.validated(lang).valueOr(err => throw new Error(s"Invalid word list: $err"))
    }

    property(s"phrases should be generated in ${lang.toString}") {
      forAll(Gen.posNum[Int]) { size =>
        val mnemonicSize = Generators.pickMnemonicSize(size)
        val entropy = Entropy.generate(mnemonicSize)

        Phrase.fromEntropy(entropy, mnemonicSize, lang).isRight shouldBe true
      }
    }
  }
}
