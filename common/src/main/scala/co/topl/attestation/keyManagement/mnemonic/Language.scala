package co.topl.attestation.keyManagement.mnemonic

import cats.implicits._
import co.topl.crypto.hash.sha256

import scala.language.implicitConversions
import scala.util.Try

/**
 * A failure to import a word list into a collection.
 */
sealed trait WordListFailure
case class FileReadFailure(exception: Throwable) extends WordListFailure
case class InvalidChecksum() extends WordListFailure

/**
 * Represents a set of 2048 words that can be used to create a mnemonic.
 *
 * @param filePath the location of the words list
 * @param hash     the SHA-256 hash of the words for verification
 */
sealed abstract class Language(val filePath: String, val hash: String) {
  import Language._

  private val wordlistDirectory: String = "bip-0039"

  /**
   * The valid set of words for the language.
   */
  lazy val words: Either[WordListFailure, WordList] =
    Try(scala.io.Source.fromResource(s"$wordlistDirectory/$filePath").getLines.toIndexedSeq).toEither
      .leftMap(FileReadFailure)
      .flatMap(validateWordList)

  /**
   * Verifies the wordlist for the given language by calculating the SHA-256 hash
   *
   * @return a validated word list or a `WordListFailure` of type `InvalidChecksum`
   */
  private def validateWordList(words: WordList): Either[WordListFailure, WordList] =
    Either.cond(
      sha256
        .hash(words.mkString.getBytes("UTF-8"))
        .value
        .map(byte => "%02x" format byte)
        .mkString == hash,
      words,
      InvalidChecksum()
    )
}

object Language {
  /**
   * Represents a list of valid words for generating a mnemonic phrase.
   */
  type WordList = IndexedSeq[String]
}

case object ChineseSimplified
    extends Language("chinese_simplified.txt", "bfd683b91db88609fabad8968c7efe4bf69606bf5a49ac4a4ba5e355955670cb")

case object ChineseTraditional
    extends Language("chinese_traditional.txt", "85b285c4e0e3eb1e52038e2cf4b4f8bba69fd814e1a09e063ce3609a1f67ad62")

case object English extends Language("english.txt", "ad90bf3beb7b0eb7e5acd74727dc0da96e0a280a258354e7293fb7e211ac03db")

case object French extends Language("french.txt", "9cbdaadbd3ce9cbaee1b360fce45e935b21e3e2c56d9fcd56b3398ced2371866")

case object Italian extends Language("italian.txt", "80d2e90d7436603fd6e57cd9af6f839391e64beac1a3e015804f094fcc5ab24c")

case object Japanese
    extends Language("japanese.txt", "d9d1fde478cbeb45c06b93632a487eefa24f6533970f866ae81f136fbf810160")

case object Korean extends Language("korean.txt", "f04f70b26cfef84474ff56582e798bcbc1a5572877d14c88ec66551272688c73")

case object Spanish extends Language("spanish.txt", "a556a26c6a5bb36db0fb7d8bf579cb7465fcaeec03957c0dda61b569962d9da5")

case object Czech extends Language("czech.txt", "f9016943461800f7870363b4c301c814dbcb8f4de801e6c87d859eba840469d5")

case object Portuguese
    extends Language("portuguese.txt", "eed387d44cf8f32f60754527e265230d8019e8a2277937c71ef812e7a46c93fd")
