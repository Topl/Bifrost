package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.generation.mnemonic.EntropyEncodeFailures.{InvalidByteSize, PhraseFailure, WordListFailure}
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.util.UUID

/**
 * Wrapper around the entropy contained represented by an array of bytes
 * @param value the underlying bytes of entropy
 */
@newtype
case class Entropy private (value: Array[Byte])

object Entropy {

  /**
   * Instantiates an 'Entropy' value from a string by validating the string to a mnemonic phrase and then deriving
   * the entropy of the string according to the BIP-39 wordlists
   * @param mnemonic string to be decoded to a mnemonic
   * @param size size of the mnemonic to try and decode
   * @param language applicable language to pull the wordlist for
   * @return either an entropy encode failure or the entropy for use in key derivation
   */
  def fromMnemonicString(
    mnemonic: String,
    size:     MnemonicSize,
    language: Language
  ): Either[EntropyEncodeFailure, Entropy] =
    for {
      wordList <- LanguageWordList.validated(language).leftMap(WordListFailure)
      phrase   <- Phrase.validated(mnemonic, size, wordList).leftMap(PhraseFailure)
      entropy = Entropy.unsafeFromPhrase(phrase, wordList, size)
    } yield entropy

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
   * Instantiates an `Entropy` value from byte data for an expected mnemonic size.
   * @param bytes the byte data to convert into entropy
   * @param size the expected size of the byte data to use for validation
   * @return either a `ValidationFailure` if the byte data is invalid or `Entropy` if it is valid
   */
  def fromBytes(bytes: Array[Byte], size: MnemonicSize): Either[EntropyEncodeFailure, Entropy] =
    Either.cond(
      bytes.length * byteLen == size.entropyLength,
      Entropy(bytes),
      InvalidByteSize
    )

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
  private def unsafeFromPhrase(phrase: Phrase, wordList: LanguageWordList, size: MnemonicSize): Entropy =
    phrase.value
      .map(wordList.value.indexOf(_))
      .map(toBinaryStringWith11Bits) // map indices to 11 bit representations
      .mkString
      .slice(0, size.entropyLength)
      .grouped(byteLen) // group into bytes
      .map(Integer.parseInt(_, 2))
      .map(_.toByte)
      .toArray
      .coerce
}

sealed trait EntropyEncodeFailure

object EntropyEncodeFailures {
  case object InvalidByteSize extends EntropyEncodeFailure
  case class PhraseFailure(failure: Phrase.ValidationFailure) extends EntropyEncodeFailure
  case class WordListFailure(failure: LanguageWordList.ValidationFailure) extends EntropyEncodeFailure
}
