package co.topl.utils

import cats.data.ValidatedNec
import cats.implicits._
import co.topl.utils.Extensions.StringOps
import co.topl.utils.encode.Base16.Base16DecodingFailure
import co.topl.utils.encode.Base58.Base58DecodingFailure
import co.topl.utils.encode.{Base16, Base58}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.language.implicitConversions

object StringDataTypes {

  type DataEncodingValidationResult[A] = ValidatedNec[DataEncodingValidationFailure, A]

  /**
   * Byte data represented by Latin-1 encoded text.
   * @param value the data bytes
   */
  case class Latin1Data private (value: Array[Byte]) {

    override def equals(obj: Any): Boolean =
      obj match {
        case o: Latin1Data => java.util.Arrays.equals(value, o.value)
        case _             => false
      }

    override def hashCode(): Int =
      java.util.Arrays.hashCode(value)
  }

  object Latin1Data {

    /**
     * Creates a `Latin1Data` instance from raw bytes data.
     * @param bytes the underlying data
     * @return a `Latin1Data` instance
     */
    def fromData(bytes: Array[Byte]): Latin1Data = Latin1Data(bytes)

    /**
     * Creates a `Latin1Data` value from a `String`.
     * Validates that the input String is a valid Latin-1 encoding.
     * @param from the `String` to create the `Latin1Data` from.
     * @return a `DataEncodingValidationResult` representing a validation error or the `Latin1Data`
     */
    def validated(from: String): DataEncodingValidationResult[Latin1Data] =
      from.getValidLatin1Bytes.toValidNec(DataEncodingValidationFailures.InvalidCharacter("")).map(Latin1Data(_))

    /**
     * Unsafely creates a `Latin1Data` instance from a `String`.
     * Throws an `IllegalArgumentException` if the input is not a valid Latin-1 encoded string.
     * @param from the `String` to create the `Latin1Data` from
     * @return the `Latin1Data`
     * @throws IllegalArgumentException when the input string is not a valid Latin-1 encoding.
     */
    def unsafe(from: String): Latin1Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Latin-1 string: $err"))
  }

  /**
   * Byte data represented by Base-58 encoded text.
   * @param value the underlying bytes data
   */
  @newtype
  class Base58Data(val value: Array[Byte])

  object Base58Data {

    /**
     * Creates `Base58Data` from the given data input.
     * @param bytes the bytes to create `Base58Data` from
     * @return a `Base58Data` instance
     */
    def fromData(bytes: Array[Byte]): Base58Data = bytes.coerce

    /**
     * Creates a `Base58Data` value from a `String`.
     * Validates that the input String is a valid Base-58 encoding.
     * @param from the `String` to create the `Base58Data` from.
     * @return a `DataEncodingValidationFailure` representing a validation error or the `Base58Data` instance
     */
    def validated(from: String): DataEncodingValidationResult[Base58Data] =
      Base58.decode(from).map(_.coerce).leftMap(DataEncodingValidationFailures.fromBase58DecodingFailure).toValidatedNec

    /**
     * Unsafely creates a `Base58Data` instance from a `String`.
     * Throws an `IllegalArgumentException` if the input is not a valid Base-58 encoded string.
     * @param from the `String` to create the `Base58Data` from
     * @return the `Base58Data`
     * @throws IllegalArgumentException when the input string is not a valid Base-58 encoding.
     */
    def unsafe(from: String): Base58Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Base-58 string: $err"))
  }

  /**
   * Byte data represented by Base-16 (Hex) encoded text.
   * @param value the data bytes
   */
  @newtype
  class Base16Data(val value: Array[Byte])

  object Base16Data {

    /**
     * Creates a `Base16Data` instance from raw bytes data.
     * @param bytes the underlying data
     * @return a `Base16Data` instance
     */
    def fromData(bytes: Array[Byte]): Base16Data = bytes.coerce

    def validated(from: String): DataEncodingValidationResult[Base16Data] =
      Base16
        .decode(from)
        .map(_.coerce)
        .leftMap(DataEncodingValidationFailures.fromBase16DecodingFailure)
        .toValidatedNec

    def unsafe(from: String): Base16Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Base-16 string: $err"))
  }

  /**
   * Represents a failure in validating that a String is of a specific encoding.
   */
  sealed trait DataEncodingValidationFailure

  object DataEncodingValidationFailures {

    /**
     * Failure when a character is not in the valid encoding set.
     */
    case class InvalidCharacter(character: String) extends DataEncodingValidationFailure

    /**
     * Failure when the length of the data is expected to be even but is not.
     */
    case object NonEvenLength extends DataEncodingValidationFailure

    def fromBase16DecodingFailure(failure: Base16DecodingFailure): DataEncodingValidationFailure = failure match {
      case Base16.InvalidCharacter(c) => InvalidCharacter(c)
      case Base16.NonEvenLength()     => NonEvenLength
    }

    def fromBase58DecodingFailure(failure: Base58DecodingFailure): DataEncodingValidationFailure = failure match {
      case Base58.InvalidCharacter(c) => InvalidCharacter(c.toString)
    }
  }

}
