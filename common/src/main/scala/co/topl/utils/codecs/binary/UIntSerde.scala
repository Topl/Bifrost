package co.topl.utils.codecs.binary

object UIntSerde {
  type UInt = Long

  def parse(from: LazyList[Byte]): ParseResult[UInt, LazyList[Byte]] =
    for {
      uLongParseResult <- ULongSerde.parse(from)
      remainingBytes = uLongParseResult._2
      uLong = uLongParseResult._1
      uInt <- Either.cond(uLong >= 0L && uLong <= 0xffffffffL, uLong, ParseFailure)
    } yield (uInt, remainingBytes)
}
