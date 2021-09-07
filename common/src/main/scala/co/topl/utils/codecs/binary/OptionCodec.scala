package co.topl.utils.codecs.binary

import cats.implicits._

object OptionCodec {

  /**
   * Decodes an `Option` value of type `T` from a lazy list of bytes.
   *
   * The type `T` must have an accompanying instance of the `LazyBytesDecoder` type-class.
   *
   * @param from the list of bytes to decode an `Option` value from
   * @tparam T the type wrapped inside of the option
   * @return if successful, a decoded `Option` value of type `T` and the remaining non-decoded bytes
   */
  def decode[T: LazyBytesDecoder](from: LazyList[Byte]): DecoderResult[Option[T]] =
    from match {
      case head #:: tail if head != 0 => LazyBytesDecoder[T].decodeLazy(tail).map(result => (result._1.some, result._2))
      case head #:: tail if head == 0 => (None, tail).asRight
      case _                          => DecoderFailure.asLeft
    }

  trait Implicits {
    implicit def lazyOptionDecoder[T: LazyBytesDecoder]: LazyBytesDecoder[Option[T]] = decode
  }
}
