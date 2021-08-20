package co.topl.models

import cats.data.ValidatedNec
import cats.implicits._
import cats.{Eq, Show}

import java.nio.charset.StandardCharsets
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
      Some(from.getBytes(StandardCharsets.ISO_8859_1))
        .filter(new String(_, StandardCharsets.ISO_8859_1) == from)
        .toValidNec(InvalidCharacter)
        .map(Latin1Data(_))

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
   * Represents a failure in validating that a String is of a specific encoding.
   */
  sealed trait DataEncodingValidationFailure

  /**
   * Failure when a character is not in the valid encoding set.
   */
  case object InvalidCharacter extends DataEncodingValidationFailure
}
