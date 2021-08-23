package co.topl.utils.codecs.binary

import cats.implicits._

object ByteStringCodec {
  def decode(from: LazyList[Byte]): DecoderResult[ByteString] =
    from match {
      case head #:: tail => stringParsingHelper(size = head & 0xff, iteration = 0, current = List(), remaining = tail)
      case _             => ParseFailure.asLeft
    }

  trait Implicits {
    implicit val lazyByteStringDecoder: LazyBytesDecoder[ByteString] = decode
  }
}
