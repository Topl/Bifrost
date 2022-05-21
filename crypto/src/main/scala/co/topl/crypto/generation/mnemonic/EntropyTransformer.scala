package co.topl.crypto.generation.mnemonic

import co.topl.crypto.generation.mnemonic.EntropyEncodeFailures.InvalidSize
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import simulacrum.{op, typeclass}

/**
 * Typeclass for encoding and decoding a value into an entropy representation
 *
 * @tparam T the type this type-class is implemented for
 */
@typeclass trait EntropyTransformer[T] {

  /**
   * Encodes a value into its entropy bytes representation
   * @param value the value to encode
   * @return the data to transmit to another node
   */
  @op("toEntropy") def encode(value: T): Either[EntropyEncodeFailure, Entropy]

  /**
   * Attempts to decode a value from its entropy representation into its expected data type.
   * @param entropy the entropy to decode into a value `T`
   * @return if successful, a value of type `T` represented by the transmitted bytes, otherwise a failure message
   */
  @op("fromEntropy") def decode(entropy: Entropy): Either[EntropyDecodeFailure, T]
}

object EntropyTransformer {

  trait Instances {

    implicit val bytesEntropyTransformer: EntropyTransformer[Array[Byte]] = new EntropyTransformer[Array[Byte]] {

      override def encode(value: Array[Byte]): Either[EntropyEncodeFailure, MnemonicSize => Entropy] =
        (size: MnemonicSize) => Either.cond(value.length * byteLen == size.entropyLength, value, InvalidSize())

      override def decode(entropy: Entropy): Either[EntropyDecodeFailure, Array[Byte]] = ???
    }
  }

  object instances extends Instances
}

/**
 * Wrapper around the entropy contained represented by an array of bytes
 * @param value the underlying bytes of entropy
 */
@newtype
case class Entropy(value: Array[Byte])

object Entropy {

  /**
   * Instantiates an `Entropy` value from byte data for an expected mnemonic size.
   * @param bytes the byte data to convert into entropy
   * @param size the expected size of the byte data to use for validation
   * @return either a `ValidationFailure` if the byte data is invalid or `Entropy` if it is valid
   */
  def validated(bytes: Array[Byte], size: MnemonicSize): Either[EntropyEncodeFailure, Entropy] =


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

sealed trait EntropyEncodeFailure

object EntropyEncodeFailures {
  case class InvalidSize() extends EntropyEncodeFailure
}

sealed trait EntropyDecodeFailure

object EntropyDecodeFailures {
  case class PhraseFailure(failure: Phrase.ValidationFailure) extends EntropyDecodeFailure
  case class WordListFailure(failure: LanguageWordList.ValidationFailure) extends EntropyDecodeFailure
}
