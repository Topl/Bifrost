package co.topl.utils.codecs.binary

import cats.implicits._

object SmallStringCodec {

  /**
   * Decodes a `SmallString` value from a set of bytes.
   * @param from the bytes to decode a `SmallString` value from
   * @return if successful, a decoded `SmallString` value and the remaining non-decoded bytes
   */
  def decode(from: Iterable[Byte]): DecoderResult[SmallString] =
    from match {
      case head :: tail =>
        stringParsingHelper(targetSize = head & 0xff, current = Array(), remaining = tail)
          .flatMap(result => SmallString.validated(result._1).map(_ -> result._2).leftMap(_ => DecoderFailure))
      case _ => DecoderFailure.asLeft
    }

  trait Implicits {
    implicit val smallStringDecoder: IterableBytesDecoder[SmallString] = decode
  }
}
