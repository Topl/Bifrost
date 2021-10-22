package co.topl.crypto.mnemonic

import co.topl.crypto.mnemonic.Language.{ChineseSimplified, ChineseTraditional, Czech, English, French, Italian, Japanese, Korean, LanguageWordList, Portuguese, Spanish}
import org.scalatest.funspec.AnyFunSpec

class LanguageSpec extends AnyFunSpec {

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

  describe("Mnemonic Language") {
    languages.foreach { lang =>
      describe(lang.toString) {
        it("should have a valid checksum with included word list") {
          LanguageWordList.validated(lang).valueOr(err => throw new Error(s"Invalid word list: $err"))
        }
      }
    }
  }
}
