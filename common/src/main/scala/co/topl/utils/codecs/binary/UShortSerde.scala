package co.topl.utils.codecs.binary

object UShortSerde {
  type UShort = Int

  def parse(from: LazyList[Byte]): ParseResult[UShort, LazyList[Byte]] =
    for {
      uLongParseResult <- ULongSerde.parse(from)
      remainingBytes = uLongParseResult._2
      intValue = uLongParseResult._1.toInt
      uShort <- Either.cond(intValue >= 0 && intValue <= 0xffff, intValue, ParseFailure)
    } yield (uShort, remainingBytes)
}
