package co.topl.utils.codecs

import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import java.nio.charset.{Charset, StandardCharsets}
import scala.language.implicitConversions

package object binary {

  type ULong = Long

  val byteSize: Int = 8

  val stringCharacterSet: Charset = StandardCharsets.UTF_8

  /**
   * Represents a string with a byte representation of 255 bytes or less.
   * @param value the string representation
   */
  @newtype
  class SmallString(val value: String)

  object SmallString {

    sealed trait ValidationFailure
    case object InvalidByteSize extends ValidationFailure

    val maxBytes: Int = 255

    def validated(from: String): Either[ValidationFailure, SmallString] = {
      val bytes = from.getBytes(stringCharacterSet)

      Either.cond(bytes.length <= maxBytes, from.coerce, InvalidByteSize)
    }

    def unsafe(from: String): SmallString =
      validated(from)
        .getOrElse(
          throw new IllegalArgumentException(
            s"value length is outside the bounds of 0 and $maxBytes"
          )
        )
  }

  /**
   * Represents a string with a byte representation of 2^31^-1 bytes or less.
   * @param value the byte representation of a UTF-8 encoded string
   */
  @newtype
  class IntString(val value: String)

  object IntString {

    sealed trait ValidationFailure
    case object InvalidByteSize extends ValidationFailure

    val maxBytes: Int = Int.MaxValue

    def validated(from: String): Either[ValidationFailure, IntString] = {
      val bytes = from.getBytes(stringCharacterSet)

      Either.cond(bytes.length <= maxBytes, from.coerce, InvalidByteSize)
    }

    def unsafe(from: String): IntString =
      validated(from)
        .getOrElse(
          throw new IllegalArgumentException(
            s"value length is outside the bounds of 0 and $maxBytes"
          )
        )
  }

  trait Implicits
      extends BooleanCodec.Implicits
      with SmallStringCodec.Implicits
      with Int128Codec.Implicits
      with IntCodec.Implicits
      with IntStringCodec.Implicits
      with LongCodec.Implicits
      with OptionCodec.Implicits
      with ShortCodec.Implicits
      with UIntCodec.Implicits
      with ULongCodec.Implicits
      with UShortCodec.Implicits

  object implicits extends Implicits

}
