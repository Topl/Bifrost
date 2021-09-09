package co.topl.utils.codecs.binary

import cats.implicits._
import scodec.{Attempt, DecodeResult, Decoder, Err}
import scodec.bits.BitVector

object OptionCodec {

  private val `0 bitVector`: BitVector = BitVector(0)

  def decode[T: Decoder](from: BitVector): Attempt[DecodeResult[Option[T]]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (byteBits, remaining) = from.splitAt(byteSize)

      if (byteBits === `0 bitVector`) Attempt.successful(DecodeResult(None, remaining))
      else Decoder[T].decode(remaining).map(_.map(_.some))
    }

  trait Implicits {
    implicit def optionDecoder[T: Decoder]: Decoder[Option[T]] = decode[T]
  }
}
