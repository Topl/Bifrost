package co.topl.utils.codecs.binary

import cats.implicits._

object BooleanCodec {

  /**
   * Decodes a `Boolean` value from a set of bytes.
   * @param from the bytes to decode a `Boolean` value from
   * @return if successful, a decoded `Boolean` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[Boolean] =
    from match {
      case head :: tail => (head == 0x01, tail).asRight
      // in the case when the byte list is empty
      case _ => DecoderFailure.asLeft
    }

  trait Implicits {
    implicit val booleanDecoder: IterableBytesDecoder[Boolean] = decode
  }
}
