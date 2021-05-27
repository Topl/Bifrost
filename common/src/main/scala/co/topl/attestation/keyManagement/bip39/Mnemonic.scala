package co.topl.attestation.keyManagement.bip39

import cats.data.ValidatedNec
import cats.implicits._
import co.topl.attestation.keyManagement.bip39.Mnemonic.MnemonicSize
import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.hash.sha256
import co.topl.utils.codecs.implicits.identityBytesEncoder
import co.topl.utils.encode.Base16

import java.util.UUID
import scala.language.implicitConversions

case class Mnemonic private (
  phrase:   String,
  entropy:  String,
  size:     MnemonicSize,
  language: Language
) {

  def toSeed(password: Option[String]): Array[Byte] =
    Pbkdf2Sha512.generateKey(
      phrase.getBytes("UTF-8"),
      ("mnemonic" + password.getOrElse("")).getBytes("UTF-8"),
      64,
      2048
    )
}

object Mnemonic {

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

  sealed abstract class MnemonicSize(val wordLength: Int) {
    val checksumLength: Int = wordLength / 3
    val entropyLength: Int = 32 * checksumLength
  }
  case object Mnemonic12 extends MnemonicSize(12)
  case object Mnemonic15 extends MnemonicSize(15)
  case object Mnemonic18 extends MnemonicSize(18)
  case object Mnemonic21 extends MnemonicSize(21)
  case object Mnemonic24 extends MnemonicSize(24)

  sealed trait CreateMnemonicFailure
  case class InvalidWordLength(length: Int) extends CreateMnemonicFailure
  case class InvalidWords(invalidWords: List[String]) extends CreateMnemonicFailure
  case class UnableToReadWordList(error: ReadWordListFailure) extends CreateMnemonicFailure
  case class InvalidChecksum() extends CreateMnemonicFailure
  case class InvalidLanguageHash() extends CreateMnemonicFailure

  def fromPhrase(phrase: String, size: MnemonicSize, language: Language): Either[CreateMnemonicFailure, Mnemonic] =
    if (!language.verifyPhraseList) InvalidLanguageHash().asLeft
    else {
      val phraseWords = phrase.toLowerCase.split(" ").map(_.trim).toList
      val languageWords = language.getWords

      languageWords
        .map(wordsList =>
          validWordLength(phraseWords, size.wordLength) match {
            case None =>
              validWords(phraseWords, wordsList) match {
                case None =>
                  validChecksum(phraseWords, wordsList, size.checksumLength, size.entropyLength) match {
                    case None =>
                      Mnemonic(
                        phraseWords.mkString(" "),
                        phraseToHex(phraseWords, size, wordsList),
                        size,
                        language
                      ).asRight
                    case Some(err) => err.asLeft
                  }
                case Some(err) => err.asLeft
              }
            case Some(err) => err.asLeft
          }
        )
        .valueOr(err => UnableToReadWordList(err).asLeft)
    }

  def fromUuid(uuid: UUID, language: Language): Either[CreateMnemonicFailure, Mnemonic] =
    fromEntropy(
      uuid.toString.filterNot("-".toSet).grouped(2).map(Integer.parseInt(_, 16).toByte).toArray,
      Mnemonic12,
      language
    )

  def fromEntropy(
    entropy:  Array[Byte],
    size:     MnemonicSize,
    language: Language
  ): Either[CreateMnemonicFailure, Mnemonic] =
    if (entropy.length * byteLen != size.entropyLength) InvalidWordLength(entropy.length).asLeft
    else if (!language.verifyPhraseList) InvalidLanguageHash().asLeft
    else {
      val languageWords = language.getWords
      val binaryString = entropy.map(toBinaryByte).mkString("")
      val binaryHashes = sha256.hash(entropy).value.map(toBinaryByte)

      languageWords.map { words =>
        Mnemonic(
          phraseFromBinaryString(binaryString, binaryHashes, size, words),
          Base16.encode(entropy),
          size,
          language
        ).asRight
      } valueOr (err => UnableToReadWordList(err).asLeft)
    }

  private def phraseFromBinaryString(
    binaryString: String,
    binaryHashes: Array[String],
    size:         MnemonicSize,
    wordList:     List[String]
  ): String =
    (binaryString + binaryHashes(0).slice(0, size.checksumLength))
      .grouped(indexLen)
      .toArray
      .map(Integer.parseInt(_, 2))
      .map(wordList(_))
      .mkString(" ")

  private def validWordLength(words: List[String], expected: Int): Option[InvalidWordLength] =
    if (words.length != expected) Some(InvalidWordLength(words.length)) else None

  private def validWords(words: List[String], expected: List[String]): Option[InvalidWords] = {
    val invalidWords = words.filterNot(expected.contains)

    if (invalidWords.isEmpty) None else Some(InvalidWords(invalidWords))
  }

  private def validChecksum(
    words:          List[String],
    wordsList:      List[String],
    checksumLength: Int,
    entropyLength:  Int
  ): Option[InvalidChecksum] = {
    val phraseBin = words.map(wordsList.indexOf(_)).map(toBinaryIndex).mkString
    val phraseHashBin: List[String] =
      sha256
        .hash(
          phraseBin.slice(0, entropyLength).grouped(byteLen).toArray map {
            Integer.parseInt(_, 2).toByte
          }
        )
        .value
        .map(toBinaryByte)
        .toList

    if (phraseBin.substring(entropyLength) == phraseHashBin.head.slice(0, checksumLength)) None
    else Some(InvalidChecksum())
  }

  private def phraseToHex(words: List[String], size: MnemonicSize, languageWords: List[String]): String = {
    val bytes = words
      .map(languageWords.indexOf(_))
      .map(toBinaryIndex)
      .mkString
      .slice(0, size.entropyLength)
      .grouped(byteLen)
      .toArray
      .map(Integer.parseInt(_, 2).toByte)

    Base16.encode(bytes)
  }
}
