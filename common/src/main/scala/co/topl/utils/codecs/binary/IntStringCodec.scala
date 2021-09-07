package co.topl.utils.codecs.binary

import cats.implicits._
import scala.util.Try
import co.topl.utils.Extensions.LongOps

object IntStringCodec {

  /**
   * Decodes an `IntString` value from a lazy list of bytes.
   * @param from the list of bytes to decode a `IntString` value from
   * @return if successful, a decoded `IntString` value and the remaining non-decoded bytes
   */
  def decode(from: LazyList[Byte]): DecoderResult[IntString] =
    for {
      // parse expected size of string
      uIntParseResult <- UIntCodec.decode(from)
      size <-
        Try(uIntParseResult._1.value.toIntExact).toEither
          .leftMap(_ => DecoderFailure)
      remainingBytes = uIntParseResult._2
      // parse string from bytes
      stringParseResult <-
        stringParsingHelper(targetSize = size, current = List(), remaining = remainingBytes)
      // validate string length
      intStringParseResult <- IntString.validated(stringParseResult._1).leftMap(_ => DecoderFailure)
    } yield intStringParseResult -> stringParseResult._2

  trait Implicits {
    implicit val lazyIntStringDecoder: LazyBytesDecoder[IntString] = decode
  }
}
