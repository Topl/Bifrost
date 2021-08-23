package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagLong

object LongSerde {

  def parse(from: LazyList[Byte]): ParseResult[Long, LazyList[Byte]] =
    for {
      uLongParseResult <- ULongSerde.parse(from)
      remainingBytes = uLongParseResult._2
      uLong = uLongParseResult._1
      long = decodeZigZagLong(uLong)
    } yield (long, remainingBytes)
}
