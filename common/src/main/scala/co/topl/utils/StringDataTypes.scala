package co.topl.utils

import cats.data.ValidatedNec
import cats.implicits._
import cats.{Eq, Show}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.encode.{Base16, Base58}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.nio.charset.StandardCharsets
import scala.language.implicitConversions

object StringDataTypes {

  type DataEncodingValidationResult[A] = ValidatedNec[DataEncodingValidationFailure, A]

  /**
   * Byte data represented by Latin-1 encoded text.
   * @param value the data bytes
   */
  @newtype
  class Latin1Data(val value: Array[Byte])

  object Latin1Data {

    /**
     * Creates a `Latin1Data` instance from raw bytes data.
     * @param bytes the underlying data
     * @return a `Latin1Data` instance
     */
    def fromData(bytes: Array[Byte]): Latin1Data = bytes.coerce

    /**
     * Creates a `Latin1Data` value from a `String`.
     * Validates that the input String is a valid Latin-1 encoding.
     * @param from the `String` to create the `Latin1Data` from.
     * @return a `DataEncodingValidationResult` representing a validation error or the `Latin1Data`
     */
    def validated(from: String): DataEncodingValidationResult[Latin1Data] =
      from.getValidLatin1Bytes.toValidNec(InvalidCharacter).map(_.coerce)

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

  trait Latin1DataInstances {
    implicit val eqLatin1Data: Eq[Latin1Data] = (a: Latin1Data, b: Latin1Data) => a.value sameElements b.value

    implicit val showLatin1Data: Show[Latin1Data] =
      latin1Data => new String(latin1Data.value, StandardCharsets.ISO_8859_1)
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
      Base58.decode(from).map(_.coerce).leftMap(_ => InvalidCharacter).toValidatedNec

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

  trait Base58DataInstances {
    implicit val eqBase58String: Eq[Base58Data] = (a: Base58Data, b: Base58Data) => a.value sameElements b.value

    implicit val showBase58String: Show[Base58Data] = data => Base58.encode(data.value)
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
      Base16.decode(from).map(_.coerce).leftMap(_ => InvalidCharacter).toValidatedNec

    def unsafe(from: String): Base16Data =
      validated(from).valueOr(err => throw new IllegalArgumentException(s"Invalid Base-16 string: $err"))
  }

  trait Base16DataInstances {

    implicit val eqBase16String: Eq[Base16Data] = (a: Base16Data, b: Base16Data) => a.value sameElements b.value

    implicit val showBase16String: Show[Base16Data] = base16Data => Base16.encode(base16Data.value)
  }

  /**
   * Represents a failure in validating that a String is of a specific encoding.
   */
  sealed trait DataEncodingValidationFailure

  /**
   * Failure when a character is not in the valid encoding set.
   */
  case object InvalidCharacter extends DataEncodingValidationFailure

  object implicits extends Latin1DataInstances with Base16DataInstances with Base58DataInstances
}
