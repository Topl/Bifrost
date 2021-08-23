package co.topl.utils.codecs.binary

object UShortCodec {
  type UShort = Int

  def decode(from: LazyList[Byte]): DecoderResult[UShort] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      intValue = uLongParseResult._1.toInt
      uShort <- Either.cond(intValue >= 0 && intValue <= 0xffff, intValue, ParseFailure)
    } yield (uShort, remainingBytes)

  trait Implicits {
    implicit def lazyUShortDecoder[T: LazyBytesDecoder]: LazyBytesDecoder[UShort] = decode
  }
}
