package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt

object IntSerde {

  def parse(from: LazyList[Byte]): ParseResult[Int, LazyList[Byte]] =
    for {
      uLongParseResult <- ULongSerde.parse(from)
      remainingBytes = uLongParseResult._2
      uLongValue = uLongParseResult._1
      intValue = decodeZigZagInt(uLongValue.toInt)
    } yield (intValue, remainingBytes)
}
