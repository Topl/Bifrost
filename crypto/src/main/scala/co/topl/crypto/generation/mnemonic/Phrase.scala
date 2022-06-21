package co.topl.crypto.generation.mnemonic

import cats.implicits._
import co.topl.crypto.generation.mnemonic.Language.LanguageWordList
import co.topl.crypto.hash.sha256

/**
 * A mnemonic phrase of words from a valid language list.
 *
 * @param value the sequence of words in the phrase
 */
case class Phrase(value: IndexedSeq[String], size: MnemonicSize, languageWords: LanguageWordList)

object Phrase {

  sealed trait ValidationFailure

  object ValidationFailures {
    case object InvalidWordLength extends ValidationFailure
    case object InvalidWords extends ValidationFailure
    case object InvalidChecksum extends ValidationFailure
    case object InvalidEntropyLength extends ValidationFailure
    case class WordListFailure(failure: LanguageWordList.ValidationFailure) extends ValidationFailure
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
      phrase <- Right(Phrase(words.toLowerCase.split("\\s+").map(_.trim).toIndexedSeq, size, languageWords))
      _      <- Either.cond(phrase.value.length == size.wordLength, phrase, ValidationFailures.InvalidWordLength)
      _      <- Either.cond(phrase.value.forall(languageWords.value.contains), phrase, ValidationFailures.InvalidWords)
      (entropyBinaryString, checksumFromPhrase) = toBinaryString(phrase)
      checksumFromSha256: String = calculateChecksum(entropyBinaryString, size)
      _ <- Either.cond(checksumFromPhrase == checksumFromSha256, phrase, ValidationFailures.InvalidChecksum)
    } yield phrase

  def fromEntropy(
    entropy:  Entropy,
    size:     MnemonicSize,
    language: Language
  ): Either[ValidationFailure, Phrase] = for {
    _ <- Either.cond(
      entropy.value.length == size.entropyLength / byteLen,
      entropy,
      ValidationFailures.InvalidEntropyLength
    )
    wordList <- LanguageWordList.validated(language).leftMap(ValidationFailures.WordListFailure)
    entropyBinaryString = entropy.value.map(byteTo8BitString).mkString
    checksum = calculateChecksum(entropyBinaryString, size)
    phrase = fromBinaryString(entropyBinaryString ++ checksum, size, wordList)
  } yield phrase

  // the phrase converted to binary with each word being 11 bits
  // 1. get index of word in wordlist
  // 2.  map indices to 11 bit representation (return List[String])
  // 3. concatenate the strings together to make a long binary string
  // 4. slice the string into the entropy + checksum pieces
  private[mnemonic] def toBinaryString(phrase: Phrase): (String, String) =
    phrase.value
      .map(phrase.languageWords.value.indexOf(_))
      .map(intTo11BitString)
      .mkString
      .splitAt(phrase.size.entropyLength)

  // the phrase converted to binary with each word being 11 bits
  // 1. get index of word in wordlist
  // 2.  map indices to 11 bit representation (return List[String])
  // 3. concatenate the strings together to make a long binary string
  // 4. slice the string into the entropy + checksum pieces
  private[mnemonic] def fromBinaryString(
    phraseBinaryString: String,
    size:               MnemonicSize,
    languageWords:      LanguageWordList
  ): Phrase = {
    val phraseWords = phraseBinaryString
      .grouped(indexLen)
      .map(Integer.parseInt(_, 2))
      .map(languageWords.value(_))
      .toIndexedSeq

    Phrase(phraseWords, size, languageWords)
  }

  // checksum section of phrase should be equal to hash of the entropy section
  private[mnemonic] def calculateChecksum(
    entropyBinaryString: String,
    size:                MnemonicSize
  ): String =
    byteTo8BitString(
      sha256
        .hash(
          // get the first `entropyLength` number of bits and hash the resulting byte array
          entropyBinaryString
            .slice(0, size.entropyLength)
            .grouped(byteLen)
            .toArray
            .map(Integer.parseInt(_, 2).toByte)
        )
        .value
        .head
    ).slice(0, size.checksumLength)
}
