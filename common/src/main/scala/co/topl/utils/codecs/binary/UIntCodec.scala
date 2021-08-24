package co.topl.utils.codecs.binary

object UIntCodec {

  /**
   * Decodes a `UInt` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `UInt` value from
   * @return if successful, a decoded `UInt` value and the remaining non-decoded bytes
   */
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
