package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object IntCodec {

  /**
   * Decodes an `Int` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `Int` value from
   * @return if successful, a decoded `Int` value and the remaining non-decoded bytes
   */
  def decode(from: LazyList[Byte]): DecoderResult[Int] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      uLongValue = uLongParseResult._1
      intValue = decodeZigZagInt(uLongValue.toInt)
    } yield (intValue, remainingBytes)

  trait Implicits {
    implicit val lazyIntDecoder: LazyBytesDecoder[Int] = decode
  }
}
