package co.topl.utils.codecs.binary

import cats.implicits._

object BooleanCodec {

  /**
   * Decodes a `Boolean` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `Boolean` value from
   * @return if successful, a decoded `Boolean` value and the remaining non-decoded bytes
   */
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
