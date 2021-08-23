package co.topl.utils.codecs.binary

import cats.implicits._

object ByteStringSerde {
  type ByteString = String

  def parse(from: LazyList[Byte]): ParseResult[ByteString, LazyList[Byte]] =
    from match {
      case head #:: tail => stringParsingHelper(size = head & 0xff, iteration = 0, current = List(), remaining = tail)
      case _             => ParseFailure.asLeft
    }
}
