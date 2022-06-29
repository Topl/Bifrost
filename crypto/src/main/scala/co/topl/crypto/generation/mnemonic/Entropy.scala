package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.generation.mnemonic.EntropyFailures.InvalidByteSize
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import co.topl.models.Bytes

import java.util.UUID

/**
 * Wrapper around the entropy contained represented by an array of bytes
 * @param value the underlying bytes of entropy
 */
case class Entropy(value: Bytes)

object Entropy {

  def generate(size: MnemonicSize = MnemonicSizes.`12`): Entropy = {
    val numBytes = size.entropyLength / byteLen
    val r = new Array[Byte](numBytes)
    new java.security.SecureRandom().nextBytes(r) // overrides r
    Entropy(Bytes(r))
  }

  def toMnemonicString(entropy: Entropy, language: Language = Language.English): Either[EntropyFailure, String] =
    for {
      size   <- sizeFromEntropyLength(entropy.value.length.toInt)
      phrase <- Phrase.fromEntropy(entropy, size, language).leftMap(EntropyFailures.PhraseToEntropyFailure)
      mnemonicString = phrase.value.mkString(" ")
    } yield mnemonicString

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
    language: Language
  ): Either[EntropyFailure, Entropy] =
    Phrase
      .validated(mnemonic, language)
      .map(Entropy.unsafeFromPhrase)
      .leftMap(EntropyFailures.PhraseToEntropyFailure)

  /**
   * Instantiates a 128-bit `Entropy` (12 word) value from a given `UUID`.
   * @param uuid a UUID to convert into `Entropy`
   * @return an `Entropy` value
   */
  def fromUuid(uuid: UUID): Entropy =
    Entropy(
      Bytes(
        uuid.toString
          .filterNot("-".toSet)
          .grouped(2)
          .map(Integer.parseInt(_, 16).toByte)
      )
    )

  /**
   * Instantiates an `Entropy` value from byte data for an expected mnemonic size.
   * @param bytes the byte data to convert into entropy
   * @param size the expected size of the byte data to use for validation
   * @return either a `ValidationFailure` if the byte data is invalid or `Entropy` if it is valid
   */
  def fromBytes(bytes: Bytes): Either[EntropyFailure, Entropy] = for {
    _ <- sizeFromEntropyLength(bytes.length.toInt)
    entropy = Entropy(bytes)
  } yield entropy

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
  private[mnemonic] def unsafeFromPhrase(phrase: Phrase): Entropy =
    Entropy(
      Bytes(
        Phrase
          .toBinaryString(phrase)
          ._1 // extract the entropy from the Phrase
          .grouped(byteLen) // group into bytes
          .map(Integer.parseInt(_, 2).toByte) // interpret the binary string as a List[Byte]
      )
    )

  private[mnemonic] def sizeFromEntropyLength(entropyByteLength: Int): Either[EntropyFailure, MnemonicSize] =
    entropyByteLength match {
      case 16 => Right(MnemonicSizes.`12`)
      case 20 => Right(MnemonicSizes.`15`)
      case 24 => Right(MnemonicSizes.`18`)
      case 28 => Right(MnemonicSizes.`21`)
      case 32 => Right(MnemonicSizes.`24`)
      case _  => Left(InvalidByteSize)
    }
}

sealed trait EntropyFailure

object EntropyFailures {
  case object InvalidByteSize extends EntropyFailure
  case class PhraseToEntropyFailure(failure: PhraseFailure) extends EntropyFailure
  case class WordListFailure(failure: LanguageWordList.ValidationFailure) extends EntropyFailure
  case object InvalidSizeMismatch extends EntropyFailure
}
