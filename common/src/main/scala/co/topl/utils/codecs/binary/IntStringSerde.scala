package co.topl.utils.codecs.binary

import co.topl.utils.Extensions.LongOps

object IntStringSerde {
  type IntString = String

  def parse(from: LazyList[Byte]): ParseResult[IntString, LazyList[Byte]] =
    for {
      uIntParseResult <- UIntSerde.parse(from)
      size = uIntParseResult._1.toIntExact
      remainingBytes = uIntParseResult._2
      stringParseResult <-
        stringParsingHelper(size = size, iteration = 0, current = List(), remaining = remainingBytes)
    } yield stringParseResult
}
