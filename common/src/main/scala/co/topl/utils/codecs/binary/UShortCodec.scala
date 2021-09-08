package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.UnsignedNumbers.UShort

object UShortCodec {

  /**
   * Decodes a `UShort` value from a set of bytes.
   * @param from the collection of bytes to decode a `UShort` value from
   * @return if successful, a decoded `UShort` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[UShort] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      intValue = uLongParseResult._1.toInt
      uShort <- UShort.validated(intValue).leftMap(_ => DecoderFailure)
    } yield (uShort, remainingBytes)

  trait Implicits {
    implicit def uShortDecoder: IterableBytesDecoder[UShort] = decode
  }
}
