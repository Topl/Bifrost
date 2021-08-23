package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Int128

import scala.annotation.tailrec

object Int128Codec {

  @tailrec
  private def decodeHelper(
    currentBytes:   Array[Byte],
    remainingBytes: LazyList[Byte],
    iteration:      Int
  ): DecoderResult[Int128] =
    if (iteration >= Int128.numBytes) (Int128(currentBytes), remainingBytes).asRight
    else
      remainingBytes match {
        case head #:: tail => decodeHelper(currentBytes :+ head, tail, iteration + 1)
        case _             => ParseFailure.asLeft
      }

  def decode(from: LazyList[Byte]): DecoderResult[Int128] =
    decodeHelper(currentBytes = Array(), remainingBytes = from, iteration = 0)

  trait Implicits {
    implicit val lazyInt128Decoder: LazyBytesDecoder[Int128] = decode
  }
}
