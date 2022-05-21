package co.topl.crypto.generation.mnemonic

import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import co.topl.crypto.hash.sha256
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

/**
 * A mnemonic phrase of words from a valid language list.
 *
 * @param value the sequence of words in the phrase
 */
@newtype
class Phrase(val value: IndexedSeq[String])

object Phrase {

  sealed trait ValidationFailure
  case class InvalidWordLength() extends ValidationFailure
  case class InvalidWords() extends ValidationFailure
  case class InvalidChecksum() extends ValidationFailure

  /**
   * Validates that the length of the given phrase matches the expected length.
   * @param phrase the mnemonic phrase
   * @param expected the expected length of the phrase
   * @return a `PhraseFailure` if invalid or the validated phrase
   */
  private def validateWordLength(
    phrase:   IndexedSeq[String],
    expected: Int
  ): Either[ValidationFailure, IndexedSeq[String]] =
    Either.cond(
      phrase.length == expected,
      phrase,
      InvalidWordLength()
    )

  /**
   * Validates that the given set of words exists in the given word list.
   * @param phrase the mnemonic phrase to validate
   * @param expected the word list to check against
   * @return a `PhraseFailure` if invalid or the validated phrase
   */
  private def validateWords(
    phrase:   IndexedSeq[String],
    expected: LanguageWordList
  ): Either[ValidationFailure, IndexedSeq[String]] =
    Either.cond(
      phrase.forall(expected.value.contains),
      phrase,
      InvalidWords()
    )

  /**
   * Validates the checksum of the given mnemonic phrase.
   * @param phrase the mnemonic phrase
   * @param wordList the BIP-0039 word list
   * @param size the expected mnemonic size
   * @return a `PhraseFailure` if invalid or the validated phrase
   */
  private def validateChecksum(
    phrase:   IndexedSeq[String],
    wordList: LanguageWordList,
    size:     MnemonicSize
  ): Either[ValidationFailure, IndexedSeq[String]] = {
    // the phrase converted to binary with each word being 11 bits
    val phraseBinary: String = phrase.map(wordList.value.indexOf(_)).map(toBinaryStringWith11Bits).mkString

    val entropyHash: List[String] =
      sha256
        .hash(
          // get the first `entropyLength` number of bits and hash the resulting byte array
          phraseBinary
            .slice(0, size.entropyLength)
            .grouped(byteLen)
            .toArray
            .map(Integer.parseInt(_, 2).toByte)
        )
        .value
        .map(toBinaryString)
        .toList

    Either.cond(
      // checksum section of phrase should be equal to hash of the entropy section
      phraseBinary.substring(size.entropyLength) == entropyHash.head.slice(0, size.checksumLength),
      phrase,
      InvalidChecksum()
    )
  }

  /**
   * Instantiates a `Phrase` from a `String` containing a list of words separated by white-space.
   *
   * @param words the words of the mnemonic phrase
   * @param size the expected size of the phrase
   * @param languageWords the word list of the language
   * @return either a `ValidationFailure` if the mnemonic phrase is invalid or a `Phrase` if it is valid
   */
  def validated(
    words:         String,
    size:          MnemonicSize,
    languageWords: LanguageWordList
  ): Either[ValidationFailure, Phrase] =
    for {
      validWordLength <-
        validateWordLength(
          words.toLowerCase.split("\\s+").map(_.trim).toIndexedSeq,
          size.wordLength
        )
      validWords    <- validateWords(validWordLength, languageWords)
      validChecksum <- validateChecksum(validWords, languageWords, size)
    } yield validChecksum.coerce
}
