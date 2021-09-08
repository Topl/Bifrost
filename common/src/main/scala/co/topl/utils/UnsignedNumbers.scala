package co.topl.utils

import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.language.implicitConversions

object UnsignedNumbers {

  /**
   * Represents an unsigned `Short` number with a max value of 65535.
   * @param value the underlying number representation as an Int value
   */
  @newtype
  class UShort(val value: Int)

  object UShort {

    sealed trait ValidationFailure
    case object OutOfBounds extends ValidationFailure

    val min: Int = 0
    val max: Int = 0xffff

    def validated(from: Int): Either[ValidationFailure, UShort] =
      Either.cond(from >= min && from <= max, from.coerce, OutOfBounds)

    def unsafe(from: Int): UShort =
      validated(from).getOrElse(
        throw new IllegalArgumentException(s"value $from is outside the bounds of $min and $max")
      )
  }

  /**
   * Represents an unsigned integer value with a maximum value of 4294967295.
   * @param value the underlying integer representation as a `Long`
   */
  @newtype
  class UInt(val value: Long)

  object UInt {

    sealed trait ValidationFailure
    case object OutOfBounds extends ValidationFailure

    val min: Long = 0
    val max: Long = 0xffffffffL

    def apply(uShort: UShort): UInt = uShort.value.toLong.coerce

    def validated(from: Long): Either[ValidationFailure, UInt] =
      Either.cond(from >= min && from <= max, from.coerce, OutOfBounds)

    def unsafe(from: Long): UInt =
      validated(from)
        .getOrElse(throw new IllegalArgumentException(s"value $from is outside the bounds of $min and $max"))
  }
}
