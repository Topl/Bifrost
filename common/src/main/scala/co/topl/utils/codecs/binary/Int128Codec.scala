package co.topl.utils.codecs.binary

import co.topl.utils.Int128
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

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

  val codec: Codec[Int128] = new Codec[Int128] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Int128]] = Int128Codec.decode(bits)

    override def encode(value: Int128): Attempt[BitVector] = Int128Codec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(int128BitSize)
  }

  trait Codecs {
    val int128: Codec[Int128] = codec
  }

  trait Implicits {
    implicit val int128ImplicitCodec: Codec[Int128] = codec
  }
}
