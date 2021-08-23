package co.topl.utils.codecs.binary

import cats.implicits._

object BooleanCodec {

  def decode(from: LazyList[Byte]): DecoderResult[Boolean] =
    from match {
      case head #:: tail => (head == 0x01, tail).asRight
      // in the case when the byte list is empty
      case _ => ParseFailure.asLeft
    }

  trait Implicits {
    implicit val lazyBooleanDecoder: LazyBytesDecoder[Boolean] = decode
  }
}
