package co.topl.utils.codecs

import cats.implicits._
import co.topl.utils.Int128
import co.topl.utils.serialization.ZigZagEncoder._
import simulacrum.typeclass
import co.topl.utils.Extensions.LongOps

import java.nio.ByteBuffer
import java.util
import scala.annotation.tailrec

package object binary {

  case object ParseFailure
  type DecoderFailure = ParseFailure.type
  type DecoderResult[T] = Either[DecoderFailure, (T, LazyList[Byte])]

  type ByteString = String
  type IntString = String

  /**
   * Helper method for recursively parsing a list of bytes into a string value.
   * @param size the number of bytes to parse into a string
   * @param iteration the current iteration count of bytes read
   * @param current the current list of bytes that will be converted into a string
   * @param remaining the remaining bytes that have not yet been parsed
   * @return a `ParseResult` which returns a string on success
   */
  @tailrec
  private[binary] def stringParsingHelper(
    size:      Int,
    iteration: Int,
    current:   List[Byte],
    remaining: LazyList[Byte]
  ): DecoderResult[String] =
    if (size >= iteration) (new String(current.toArray), remaining).asRight
    else
      remaining match {
        case head #:: tail => stringParsingHelper(size, iteration + 1, current :+ head, tail)
        case _             => ParseFailure.asLeft
      }

  trait Implicits
      extends LazyBytesDecoder.Implicits
      with LazyBytesDecoder.ToLazyBytesDecoderOps
      with BooleanCodec.Implicits
      with ByteStringCodec.Implicits
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
