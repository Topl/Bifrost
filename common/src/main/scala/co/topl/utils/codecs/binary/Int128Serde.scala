package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Int128

import scala.annotation.tailrec

class Int128Serde {

  @tailrec
  private def parseHelper(
    currentBytes:   Array[Byte],
    remainingBytes: LazyList[Byte],
    iteration:      Int
  ): ParseResult[Int128, LazyList[Byte]] =
    if (iteration >= Int128.numBytes) (Int128(currentBytes), remainingBytes).asRight
    else
      remainingBytes match {
        case head #:: tail => parseHelper(currentBytes :+ head, tail, iteration + 1)
        case _             => ParseFailure.asLeft
      }

  def parse(from: LazyList[Byte]): ParseResult[Int128, LazyList[Byte]] =
    parseHelper(currentBytes = Array(), remainingBytes = from, iteration = 0)
}
