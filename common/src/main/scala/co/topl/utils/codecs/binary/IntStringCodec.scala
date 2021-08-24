package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Extensions.LongOps
import scala.util.Try

object IntStringCodec {

  /**
   * Decodes an `IntString` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `IntString` value from
   * @return if successful, a decoded `IntString` value and the remaining non-decoded bytes
   */
  def decode(from: LazyList[Byte]): DecoderResult[IntString] =
    for {
      uIntParseResult <- UIntCodec.decode(from)
      size <-
        Try(uIntParseResult._1.toIntExact).toEither
          .leftMap(_ => ParseFailure)
      remainingBytes = uIntParseResult._2
      stringParseResult <-
        stringParsingHelper(targetSize = size, current = List(), remaining = remainingBytes)
    } yield stringParseResult

  trait Implicits {
    implicit val lazyIntStringDecoder: LazyBytesDecoder[IntString] = decode
  }
}
