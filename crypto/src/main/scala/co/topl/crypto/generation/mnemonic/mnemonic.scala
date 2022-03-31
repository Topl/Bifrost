package co.topl.crypto.generation

import co.topl.crypto.hash.sha256
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.util.UUID
import scala.language.implicitConversions
import scala.math.BigInt

/**
 * A mnemonic represents a set of random entropy that can be used to derive a private key or other type of value.
 * This implementation follows a combination of BIP-0039 and SLIP-0023.
 * https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
 * https://github.com/satoshilabs/slips/blob/master/slip-0023.md
 */
package object mnemonic {

  /*
   * ENT = entropy
   * CS (checksum) = ENT / 32
   * MS (mnemonic size) = (ENT + CS) / 11
   *
   * |  ENT  | CS | ENT+CS |  MS  |
   * +-------+----+--------+------+
   * |  128  |  4 |   132  |  12  |
   * |  160  |  5 |   165  |  15  |
   * |  192  |  6 |   198  |  18  |
   * |  224  |  7 |   231  |  21  |
   * |  256  |  8 |   264  |  24  |
   *
   */

  private[mnemonic] val byteLen = 8
  private[mnemonic] val indexLen = 11

  /**
   * Converts an integer into a binary representation with 11 bits.
   * @param i the index to convert
   * @return the 11-bit binary representation as a `String`
   */
  private[mnemonic] def toBinaryStringWith11Bits(i: Int): String =
    String.format("%11s", BigInt(i).toString(2)).replace(' ', '0')

  /**
   * Converts a byte to a binary string.
   * @param b the byte to convert
   * @return the binary representation as a `String`
   */
  private[mnemonic] def toBinaryString(b: Byte): String =
    String.format("%8s", BigInt(b & 0xff).toString(2)).replace(' ', '0')

  /**
   * Mnemonic size is an enum with additional parameters for calculating checksum and entropy lengths.
   *
   * @param wordLength the size of the mnemonic
   */
  sealed abstract class MnemonicSize(val wordLength: Int) {
    val checksumLength: Int = wordLength / 3
    val entropyLength: Int = 32 * checksumLength
  }

  object MnemonicSize {
    case object Mnemonic12 extends MnemonicSize(12)
    case object Mnemonic15 extends MnemonicSize(15)
    case object Mnemonic18 extends MnemonicSize(18)
    case object Mnemonic21 extends MnemonicSize(21)
    case object Mnemonic24 extends MnemonicSize(24)
  }

  /**
   * The underlying entropy of a mnemonic value.
   * @param value the underlying bytes of entropy
   */
  @newtype
  case class Entropy(value: Array[Byte])

  object Entropy {

    sealed trait ValidationFailure
    case class InvalidSize() extends ValidationFailure

    /**
     * Instantiates an `Entropy` value from byte data for an expected mnemonic size.
     * @param bytes the byte data to convert into entropy
     * @param size the expected size of the byte data to use for validation
     * @return either a `ValidationFailure` if the byte data is invalid or `Entropy` if it is valid
     */
    def validated(bytes: Array[Byte], size: MnemonicSize): Either[ValidationFailure, Entropy] =
      Either.cond(bytes.length * byteLen == size.entropyLength, bytes.coerce, InvalidSize())

    /**
     * Instantiates an `Entropy` value from a given `UUID`.
     * @param uuid a UUID to convert into `Entropy`
     * @return an `Entropy` value
     */
    def fromUuid(uuid: UUID): Entropy =
      uuid.toString
        .filterNot("-".toSet)
        .grouped(2)
        .map(Integer.parseInt(_, 16).toByte)
        .toArray
        .coerce

    /**
     * Instantiates an `Entropy` value from a `Phrase`, `LanguageWordList`, and `MnemonicSize`.
     *
     * Note: the phrase is not re-validated for the given `LanguageWordList` and `MnemonicSize`
     *
     * @param phrase the mnemonic phrase to get entropy from
     * @param wordList the list of valid mnemonic words for the language
     * @param size the mnemonic size of the phrase
     * @return the underlying entropy of the mnemonic phrase
     */
    def fromPhrase(phrase: Phrase, wordList: LanguageWordList, size: MnemonicSize): Entropy =
      phrase.value
        .map(wordList.value.indexOf(_))
        // map indices to 11 bit representations
        .map(toBinaryStringWith11Bits)
        .mkString
        .slice(0, size.entropyLength)
        // group into bytes
        .grouped(byteLen)
        .map(Integer.parseInt(_, 2))
        .map(_.toByte)
        .toArray
        .coerce
  }

  /**
   * A mnemonic phrase of words from a valid language list.
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
}
