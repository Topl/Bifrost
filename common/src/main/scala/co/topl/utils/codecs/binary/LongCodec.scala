package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagLong

object LongCodec {

  /**
   * Decodes a `Long` value from a set of bytes.
   * @param from the bytes to decode a `Long` value from
   * @return if successful, a decoded `Long` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[Long] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      uLong = uLongParseResult._1
      long = decodeZigZagLong(uLong)
    } yield (long, remainingBytes)

  trait Implicits {
    implicit val longDecoder: IterableBytesDecoder[Long] = decode
  }
}
