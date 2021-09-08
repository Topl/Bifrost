package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object IntCodec {

  /**
   * Decodes an `Int` value from a set of bytes.
   * @param from the bytes to decode an `Int` value from
   * @return if successful, a decoded `Int` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[Int] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      uLongValue = uLongParseResult._1
      intValue = decodeZigZagInt(uLongValue.toInt)
    } yield (intValue, remainingBytes)

  trait Implicits {
    implicit val intDecoder: IterableBytesDecoder[Int] = decode
  }
}
