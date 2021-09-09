package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.UnsignedNumbers.UInt
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}
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

  def encode(value: UInt): Attempt[BitVector] = ULongCodec.encode(value.value)

  trait Implicits {
    implicit val uIntDecoder: Decoder[UInt] = decode

    implicit val uIntEncoder: Encoder[UInt] = new Encoder[UInt] {
      override def encode(value: UInt): Attempt[BitVector] = UIntCodec.encode(value)

      override def sizeBound: SizeBound = SizeBound.atMost(64)
    }

    implicit val uIntCoded: Codec[UInt] = Codec(uIntEncoder, uIntDecoder)
  }
}
