package co.topl.utils.codecs.binary

import cats.implicits._

object OptionCodec {

  def decode[T: LazyBytesDecoder](from: LazyList[Byte]): DecoderResult[Option[T]] =
    from match {
      case head #:: tail if head != 0 => LazyBytesDecoder[T].decodeLazy(tail).map(result => (result._1.some, result._2))
      case head #:: tail if head == 0 => (None, tail).asRight
      case _                          => ParseFailure.asLeft
    }

  trait Implicits {
    implicit def lazyOptionDecoder[T: LazyBytesDecoder]: LazyBytesDecoder[Option[T]] = decode
  }
}
