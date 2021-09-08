package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object ShortCodec {

  /**
   * Decodes a `Short` value from a set of bytes.
   * @param from the bytes to decode a `Short` value from
   * @return if successful, a decoded `Short` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[Short] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      intValue = uLongParseResult._1.toInt
      short = decodeZigZagInt(intValue).toShort
    } yield (short, remainingBytes)

  trait Implicits {
    implicit def shortDecoder: IterableBytesDecoder[Short] = decode
  }
}
