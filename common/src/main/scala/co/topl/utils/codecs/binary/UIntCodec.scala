package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.UnsignedNumbers.UInt
import scodec.{Attempt, DecodeResult, Decoder, Err}
import scodec.bits.BitVector

object UIntCodec {

  def decode(from: BitVector): Attempt[DecodeResult[UInt]] =
    ULongCodec
      .decode(from)
      .flatMap(result =>
        Attempt.fromEither(
          UInt
            .validated(result.value)
            .leftMap(failure => Err(failure.toString))
            .map(DecodeResult(_, result.remainder))
        )
      )

  trait Implicits {
    implicit def uIntDecoder: Decoder[UInt] = decode
  }
}
