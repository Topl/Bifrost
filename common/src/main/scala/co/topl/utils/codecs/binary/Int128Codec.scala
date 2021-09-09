package co.topl.utils.codecs.binary

import co.topl.utils.Int128
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

object Int128Codec {

  private val int128BitSize = Int128.numBytes * byteSize

  def decode(from: BitVector): Attempt[DecodeResult[Int128]] =
    if (from.length < int128BitSize)
      Attempt.failure(Err.insufficientBits(int128BitSize, from.length))
    else {
      val (int128Bits, remainder) = from.splitAt(int128BitSize)
      Attempt.successful(DecodeResult(Int128(int128Bits.toByteArray), remainder))
    }

  def encode(value: Int128): Attempt[BitVector] = Attempt.successful(BitVector(value.toByteArray))

  trait Implicits {
    implicit val int128Decoder: Decoder[Int128] = decode

    implicit val int128Encoder: Encoder[Int128] = new Encoder[Int128] {
      override def encode(value: Int128): Attempt[BitVector] = Int128Codec.encode(value)

      override def sizeBound: SizeBound = SizeBound.exact(int128BitSize)
    }

    implicit val in128Codec: Codec[Int128] = Codec(int128Encoder, int128Decoder)
  }
}
