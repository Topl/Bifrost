package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object ShortSerde {

  def parse(from: LazyList[Byte]): ParseResult[Short, LazyList[Byte]] =
    for {
      uLongParseResult <- ULongSerde.parse(from)
      remainingBytes = uLongParseResult._2
      intValue = uLongParseResult._1.toInt
      short = decodeZigZagInt(intValue).toShort
    } yield (short, remainingBytes)
}
