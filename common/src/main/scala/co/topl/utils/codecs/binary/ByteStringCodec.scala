package co.topl.utils.codecs.binary

import cats.implicits._

object ByteStringCodec {

  /**
   * Decodes a `ByteString` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `ByteString` value from
   * @return if successful, a decoded `ByteString` value and the remaining non-decoded bytes
   */
  def decode(from: LazyList[Byte]): DecoderResult[ByteString] =
    from match {
      case head #:: tail => stringParsingHelper(targetSize = head & 0xff, current = List(), remaining = tail)
      case _             => ParseFailure.asLeft
    }

  trait Implicits {
    implicit val lazyByteStringDecoder: LazyBytesDecoder[ByteString] = decode
  }
}
