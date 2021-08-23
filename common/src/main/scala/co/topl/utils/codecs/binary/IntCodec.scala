package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object IntCodec {

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
