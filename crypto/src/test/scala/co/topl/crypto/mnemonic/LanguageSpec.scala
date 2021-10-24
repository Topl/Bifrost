package co.topl.crypto.mnemonic

import cats.implicits._
import co.topl.crypto.mnemonic.Language._
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
