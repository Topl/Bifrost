package co.topl.attestation.keyManagement.mnemonicSeed

import cats.implicits._
import co.topl.attestation.keyManagement.derivedKeys.ExtendedPrivateKeyEd25519
import co.topl.crypto.Pbkdf2Sha512
import co.topl.crypto.hash.sha256
import co.topl.utils.SizedByteCollection
import co.topl.utils.SizedByteCollection.implicits._
import co.topl.utils.SizedByteCollection.Types.ByteVector32
import co.topl.utils.encode.Base16
import scodec.bits.ByteOrdering

import java.util.UUID
import scala.language.implicitConversions

/**
 * A mnemonic represents a set of entropy that can be used to derive a private key given an optional password.
 * This implementation follows a combination of BIP-0039 and SLIP-0023.
 * https://github.com/bitcoin/bips/blob/master/bip-0039.mediawiki
 * https://github.com/satoshilabs/slips/blob/master/slip-0023.md
 */
object Mnemonic {

  type Password = String

  /**
   * A mnemonic represents a function that takes in an optional password and returns an extended private key.
   */
  type Mnemonic = Password => ExtendedPrivateKeyEd25519

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
   * Creates a validated mnemonic that can be used to derive an extended private key.
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
    } yield mnemonicFromValidatedPhrase(validChecksum, wordList, size)

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
    } yield mnemonicFromValidatedPhrase(phrase, wordList, size)

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
  ): IndexedSeq[String] =
    (binaryString + binaryHashes(0).slice(0, size.checksumLength))
      .grouped(indexLen)
      .toArray
      .map(Integer.parseInt(_, 2))
      .map(wordList(_))

  /**
   * Validates that the length of the given mnemonic phrase matches the expected length.
   * @param words the mnemonic phrase
   * @param expected the expected length of the phrase
   * @return a `CreateMnemonicFailure` if invalid or the validated phrase
   */
  private def validateWordLength(
    words:    IndexedSeq[String],
    expected: Int
  ): Either[CreateMnemonicFailure, IndexedSeq[String]] =
    Either.cond(
      words.length == expected,
      words,
      InvalidWordLength()
    )

  /**
   * Validates that the given set of words exists in the given word list.
   * @param words the mnemonic phrase to validate
   * @param expected the word list to check against
   * @return a `CreateMnemonicFailure` if invalid or the validated phrase
   */
  private def validateWords(
    words:    IndexedSeq[String],
    expected: IndexedSeq[String]
  ): Either[CreateMnemonicFailure, IndexedSeq[String]] =
    Either.cond(
      words.forall(expected.contains),
      words,
      InvalidWords()
    )

  /**
   * Validates the checksum of the given mnemonic phrase.
   * @param words the mnemonic phrase
   * @param wordsList the BIP-0039 word list
   * @param checksumLength the length of the mnemonic checksum
   * @param entropyLength the length of the mnemonic entropy
   * @return a `CreateMnemonicFailure` if invalid or the validated phrase
   */
  private def validateChecksum(
    words:          IndexedSeq[String],
    wordsList:      IndexedSeq[String],
    checksumLength: Int,
    entropyLength:  Int
  ): Either[CreateMnemonicFailure, IndexedSeq[String]] = {
    val phraseBin: String = words.map(wordsList.indexOf(_)).map(toBinaryIndex).mkString
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
   * @param validatedPhrase the validated mnemonic phrase
   * @param wordList the word list to use
   * @param size te size of the mnemonic
   * @return a `Mnemonic` value that can be used to derive an extended private key
   */
  private def mnemonicFromValidatedPhrase(
    validatedPhrase: IndexedSeq[String],
    wordList:        IndexedSeq[String],
    size:            MnemonicSize
  ): Mnemonic =
    (password: String) => {
      val entropy = entropyFromValidatedPhrase(validatedPhrase, wordList, size)

      val seed = Pbkdf2Sha512.generateKey(
        password.getBytes,
        entropy,
        96,
        4096
      )

      // turn seed into a valid ExtendedPrivateKeyEd25519 per the SLIP-0023 spec
      seed(0) = (seed(0) & 0xf8).toByte
      seed(31) = ((seed(31) & 0x1f) | 0x40).toByte

      ExtendedPrivateKeyEd25519(
        SizedByteCollection[ByteVector32].fit(seed.slice(0, 32), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(seed.slice(32, 64), ByteOrdering.LittleEndian),
        SizedByteCollection[ByteVector32].fit(seed.slice(64, 96), ByteOrdering.LittleEndian),
        Seq()
      )
    }

  /**
   * Creates a byte array of entropy from a valid mnemonic phrase.
   * @param words the mnemonic phrase
   * @param wordsList the word list
   * @param size the mnemonic size
   * @return the entropy representing the mnemonic phrase
   */
  private def entropyFromValidatedPhrase(
    words:     IndexedSeq[String],
    wordsList: IndexedSeq[String],
    size:      MnemonicSize
  ): Array[Byte] =
    words
      .map(wordsList.indexOf(_))
      .map(toBinaryIndex)
      .mkString
      .slice(0, size.entropyLength)
      .grouped(byteLen)
      .map(Integer.parseInt(_, 2))
      .map(_.toByte)
      .toArray
}
