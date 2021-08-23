package co.topl.utils.codecs.binary

import co.topl.utils.Extensions.LongOps

object IntStringCodec {

  def decode(from: LazyList[Byte]): DecoderResult[IntString] =
    for {
      uIntParseResult <- UIntCodec.decode(from)
      size = uIntParseResult._1.toIntExact
      remainingBytes = uIntParseResult._2
      stringParseResult <-
        stringParsingHelper(size = size, iteration = 0, current = List(), remaining = remainingBytes)
    } yield stringParseResult

  trait Implicits {
    implicit val lazyIntStringDecoder: LazyBytesDecoder[IntString] = decode
  }
}
