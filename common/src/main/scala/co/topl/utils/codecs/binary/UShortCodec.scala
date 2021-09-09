package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.UnsignedNumbers.UShort
import scodec.{Attempt, DecodeResult, Decoder, Err}
import scodec.bits.BitVector

object UShortCodec {

  def decode(from: BitVector): Attempt[DecodeResult[UShort]] =
    ULongCodec
      .decode(from)
      .flatMap(result =>
        Attempt.fromEither(
          UShort
            .validated(result.value.toInt)
            .leftMap(failure => Err(failure.toString))
            .map(DecodeResult(_, result.remainder))
        )
      )

  trait Implicits {
    implicit def uShortDecoder: Decoder[UShort] = decode
  }
}
