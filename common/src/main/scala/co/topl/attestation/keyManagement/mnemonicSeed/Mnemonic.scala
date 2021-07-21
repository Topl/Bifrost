package co.topl.attestation.keyManagement.mnemonicSeed

import cats.implicits._
import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.hash.sha256

import java.util.UUID
import scala.language.implicitConversions

// mnemonic seed protocol follows BIP-39 -> https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
object Mnemonic {

  type Password = String
  type Seed = Array[Byte]

  /**
   * A mnemonic represents a function that takes in an optional password and returns a seed
   */
  type Mnemonic = Option[Password] => Seed

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

  /**
   * Mnemonic size is an enum with additional parameters for calculating checksum and entropy lengths.
   *
   * @param wordLength the size of the mnemonic
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
  case class InvalidWordLength() extends CreateMnemonicFailure
  case class InvalidWords() extends CreateMnemonicFailure
  case class BadWordList(error: WordListFailure) extends CreateMnemonicFailure
  case class InvalidChecksum() extends CreateMnemonicFailure
  case class InvalidLanguageHash() extends CreateMnemonicFailure

  /**
   * Creates a validated BIP-39 mnemonic of the given size and language.
   *
   * @param phrase   the mnemonic phrase
   * @param size     the size of the mnemonic phrase
   * @param language the language to pull the word list for
   * @return either a `CreateMnemonicFailure` or a `Mnemonic` value
   */
  def fromPhrase(phrase: String, size: MnemonicSize, language: Language): Either[CreateMnemonicFailure, Mnemonic] =
    for {
      wordList <- language.words.leftMap(BadWordList)
      // split on whitespace
      phraseWords = phrase.toLowerCase.split("\\s+").map(_.trim).toIndexedSeq
      validWordLength <- validateWordLength(phraseWords, size.wordLength)
      validWords      <- validateWords(validWordLength, wordList)
      validChecksum   <- validateChecksum(validWords, wordList, size.checksumLength, size.entropyLength)
    } yield mnemonicFromValidatedPhrase(validChecksum.mkString(" "))

  /**
   * Creates a mnemonic from the byte representation of a given UUID.
   *
   * @param uuid     the UUID to convert into entropy
   * @param language the language of the mnemonic
   * @return either a `CreateMnemonicFailure` or a valid `Mnemonic` of size `Mnemonic12`
   */
  def fromUuid(uuid: UUID, language: Language): Either[CreateMnemonicFailure, Mnemonic] =
    fromEntropy(
      uuid.toString.filterNot("-".toSet).grouped(2).map(Integer.parseInt(_, 16).toByte).toArray,
      Mnemonic12,
      language
    )

  /**
   * Creates a mnemonic from the given entropy byte array.
   *
   * @param entropy  the entropy byte array
   * @param size     the expected size of the entropy
   * @param language the language to create the mnemonic from
   * @return either a `CreateMnemonicFailure` or a valid mnemonic of the given size
   */
  def fromEntropy(
    entropy:  Array[Byte],
    size:     MnemonicSize,
    language: Language
  ): Either[CreateMnemonicFailure, Mnemonic] =
    for {
      validEntropy <- Either.cond(entropy.length * byteLen == size.entropyLength, entropy, InvalidWordLength())
      wordList     <- language.words.leftMap(BadWordList)
      binaryString = validEntropy.map(toBinaryByte).mkString("")
      binaryHashes = sha256.hash(validEntropy).value.map(toBinaryByte)
      phrase = phraseFromBinaryString(binaryString, binaryHashes, size, wordList)
    } yield mnemonicFromValidatedPhrase(phrase)

  /**
   * Creates a mnemonic phrase from the given string of binary numbers (0,1) and binary hashes of the original entropy
   * bytes.
   *
   * @param binaryString the binary string to convert to a mnemonic
   * @param binaryHashes the hashes of the original entropy bytes
   * @param size         the size of the mnemonic
   * @param wordList     the word list to create the mnemonic from
   * @return a mnemonic phrase
   */
  private def phraseFromBinaryString(
    binaryString: String,
    binaryHashes: Array[String],
    size:         MnemonicSize,
    wordList:     IndexedSeq[String]
  ): String =
    (binaryString + binaryHashes(0).slice(0, size.checksumLength))
      .grouped(indexLen)
      .toArray
      .map(Integer.parseInt(_, 2))
      .map(wordList(_))
      .mkString(" ")

  private def validateWordLength(
    words:    IndexedSeq[String],
    expected: Int
  ): Either[CreateMnemonicFailure, IndexedSeq[String]] =
    Either.cond(
      words.length == expected,
      words,
      InvalidWordLength()
    )

  private def validateWords(
    words:    IndexedSeq[String],
    expected: IndexedSeq[String]
  ): Either[CreateMnemonicFailure, IndexedSeq[String]] =
    Either.cond(
      words.forall(expected.contains),
      words,
      InvalidWords()
    )

  private def validateChecksum(
    words:          IndexedSeq[String],
    wordsList:      IndexedSeq[String],
    checksumLength: Int,
    entropyLength:  Int
  ): Either[CreateMnemonicFailure, IndexedSeq[String]] = {
    val phraseBin = words.map(wordsList.indexOf(_)).map(toBinaryIndex).mkString
    val phraseHashBin: List[String] =
      sha256
        .hash(
          phraseBin
            .slice(0, entropyLength)
            .grouped(byteLen)
            .toArray
            .map(Integer.parseInt(_, 2).toByte)
        )
        .value
        .map(toBinaryByte)
        .toList

    Either.cond(
      phraseBin.substring(entropyLength) == phraseHashBin.head.slice(0, checksumLength),
      words,
      InvalidChecksum()
    )
  }

  /**
   * Creates a mnemonic from a pre-validated phrase.
   *
   * @param validatedPhrase the valid phrase to create a mnemonic from
   * @return the mnemonic function
   */
  private def mnemonicFromValidatedPhrase(validatedPhrase: String): Mnemonic =
    (password: Option[String]) =>
      Pbkdf2Sha512.generateKey(
        validatedPhrase.getBytes("UTF-8"),
        ("mnemonic" + password.getOrElse("")).getBytes("UTF-8"),
        64,
        2048
      )
}
