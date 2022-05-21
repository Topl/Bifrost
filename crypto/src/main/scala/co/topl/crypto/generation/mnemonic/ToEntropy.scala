package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.generation.mnemonic.EntropyDecodeFailures.{PhraseFailure, WordListFailure}
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import co.topl.crypto.signing.{Curve25519, ExtendedEd25519}
import co.topl.models.SecretKeys
import simulacrum.typeclass

import java.util.UUID

/**
 * Represents a value that can be generated from random entropy.
 * @tparam T the type of the value
 */
@typeclass
trait ToEntropy[T] {

  /**
   * Derives a value of `T` from some random entropy.
   * @param entropy the entropy used to generate a value of `T`
   * @return a value of `T`
   */
  def deriveFrom(entropy: Entropy): T
}

object ToEntropy {

  /**
   * Derives a value `T` from the set of entropy underlying the given mnemonic.
   *
   * @param mnemonic a mnemonic phrase containing a set of words separated by white-space
   * @param size     the expected size of the mnemonic
   * @param language the language that the words in the phrase come from
   * @tparam T the value to create from the generated entropy
   * @return either a `DeriveFailure` if a failure occurred or a value `T`
   */
  def derive[T: ToEntropy](mnemonic: String, size: MnemonicSize, language: Language): Either[EntropyDecodeFailure, T] =
    for {
      wordList <- LanguageWordList.validated(language).leftMap(WordListFailure)
      phrase   <- Phrase.validated(mnemonic, size, wordList).leftMap(PhraseFailure)
      entropy = Entropy.fromPhrase(phrase, wordList, size)
    } yield ToEntropy[T].deriveFrom(entropy)

  def derive[T: ToEntropy](bytes: Array[Byte], size: MnemonicSize): Either[EntropyDecodeFailure, T] =
    Entropy.validated(bytes, size).leftMap(EntropyFailure).map(ToEntropy[T].deriveFrom)

  def derive[T: ToEntropy](uuid: UUID): T = ToEntropy[T].deriveFrom(Entropy.fromUuid(uuid))

  trait Instances {

    implicit val stringToExtended25519FromEntropy: ToEntropy[String => SecretKeys.ExtendedEd25519] = {
      entropy => password => ExtendedEd25519.fromEntropy(entropy)(password)
    }
  }

  object Instances extends Instances
}
