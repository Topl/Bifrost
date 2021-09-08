package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Int128

import scala.annotation.tailrec

object Int128Codec {

  /**
   * Recursive helper method for decoding an `Int128` value from an iterable of bytes.
   *
   * @param currentBytes the current collection of bytes which will be converted into an `Int128` value
   * @param remainingBytes the remaining bytes to parse
   * @return if successful, a decoded `Int128` value and the remaining non-decoded bytes
   */
  @tailrec
  private def decodeHelper(
    currentBytes:   Array[Byte],
    remainingBytes: Iterable[Byte]
  ): DecoderResult[Int128] =
    if (currentBytes.length >= Int128.numBytes) (Int128(currentBytes), remainingBytes).asRight
    else
      remainingBytes match {
        case head :: tail => decodeHelper(currentBytes :+ head, tail)
        case _            => DecoderFailure.asLeft
      }

  /**
   * Decodes an `Int128` value from a set of bytes.
   * @param from the bytes to decode an `Int128` value from
   * @return if successful, a decoded `Int128` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[Int128] = decodeHelper(currentBytes = Array(), remainingBytes = from)

  trait Implicits {
    implicit val int128Decoder: IterableBytesDecoder[Int128] = decode
  }
}
