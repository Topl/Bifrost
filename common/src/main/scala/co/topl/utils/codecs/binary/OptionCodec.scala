package co.topl.utils.codecs.binary

import cats.implicits._

object OptionCodec {

  /**
   * Decodes an `Option` value of type `T` from a set of bytes.
   *
   * The type `T` must have an accompanying instance of the `IterableBytesDecoder` type-class.
   *
   * @param from the bytes to decode an `Option` value from
   * @tparam T the type wrapped inside of the option
   * @return if successful, a decoded `Option` value of type `T` and the remaining non-decoded bytes
   */
  def decode[T: IterableBytesDecoder](from: Iterable[Byte]): DecoderResult[Option[T]] =
    from match {
      case head :: tail if head != 0 => IterableBytesDecoder[T].decode(tail).map(result => (result._1.some, result._2))
      case head :: tail if head == 0 => (None, tail).asRight
      case _                         => DecoderFailure.asLeft
    }

  trait Implicits {
    implicit def optionDecoder[T: IterableBytesDecoder]: IterableBytesDecoder[Option[T]] = decode
  }
}
