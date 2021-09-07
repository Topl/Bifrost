package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.UnsignedNumbers.UInt

object UIntCodec {

  /**
   * Decodes a `UInt` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `UInt` value from
   * @return if successful, a decoded `UInt` value and the remaining non-decoded bytes
   */
  def decode(from: LazyList[Byte]): DecoderResult[UInt] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      uLong = uLongParseResult._1
      uInt <- UInt.validated(uLong).leftMap(_ => DecoderFailure)
    } yield (uInt, remainingBytes)

  trait Implicits {
    implicit def lazyUIntDecoder: LazyBytesDecoder[UInt] = decode
  }
}
