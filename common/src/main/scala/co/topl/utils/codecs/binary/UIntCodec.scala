package co.topl.utils.codecs.binary

object UIntCodec {
  type UInt = Long

  def decode(from: LazyList[Byte]): DecoderResult[UInt] =
    for {
      uLongParseResult <- ULongCodec.decode(from)
      remainingBytes = uLongParseResult._2
      uLong = uLongParseResult._1
      uInt <- Either.cond(uLong >= 0L && uLong <= 0xffffffffL, uLong, ParseFailure)
    } yield (uInt, remainingBytes)

  trait Implicits {
    implicit def lazyUIntDecoder: LazyBytesDecoder[UInt] = decode
  }
}
