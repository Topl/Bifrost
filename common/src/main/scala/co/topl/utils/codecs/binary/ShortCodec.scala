package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object ShortCodec {

  def decode(from: LazyList[Byte]): DecoderResult[Short] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      intValue = uLongParseResult._1.toInt
      short = decodeZigZagInt(intValue).toShort
    } yield (short, remainingBytes)

  trait Implicits {
    implicit def lazyShortDecoder: LazyBytesDecoder[Short] = decode
  }
}
