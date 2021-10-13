package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.Int128
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object Int128Codec {

  private val int128BitSize = Int128.numBytes * byteSize

  /**
   * Attempts to decode an `Int128` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, an `Int128` and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[Int128]] =
    if (from.length < int128BitSize)
      Attempt.failure(Err.insufficientBits(int128BitSize, from.length))
    else {
      val (int128Bits, remainder) = from.splitAt(int128BitSize)
      Attempt.successful(DecodeResult(Int128(int128Bits.toByteArray), remainder))
    }

  /**
   * Attempts to encode an `Int128` number as a vector of bits.
   * @param value the value to encode
   * @return if successful, a vector of encoded bits, otherwise an error
   */
  def encode(value: Int128): Attempt[BitVector] = Attempt.successful(BitVector(value.toByteArray))

  /**
   * Codec type-class for encoding/decoding an `Int128` value to/from a bit vector.
   */
  val codec: Codec[Int128] = new Codec[Int128] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Int128]] = Int128Codec.decode(bits)

    override def encode(value: Int128): Attempt[BitVector] = Int128Codec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(int128BitSize)
  }

  trait Codecs {
    implicit val int128Codec: Codec[Int128] = codec
  }

  object codecs extends Codecs
}
