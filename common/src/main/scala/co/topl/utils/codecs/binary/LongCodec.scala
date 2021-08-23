package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagLong

object LongCodec {

  def decode(from: LazyList[Byte]): DecoderResult[Long] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      uLong = uLongParseResult._1
      long = decodeZigZagLong(uLong)
    } yield (long, remainingBytes)

  trait Implicits {
    implicit val lazyLongDecoder: LazyBytesDecoder[Long] = decode
  }
}
