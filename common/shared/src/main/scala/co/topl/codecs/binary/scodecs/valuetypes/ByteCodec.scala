package co.topl.codecs.binary.scodecs.valuetypes

import co.topl.codecs.binary.scodecs.valuetypes.Constants.byteSize
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object ByteCodec extends Codec[Byte] {

  /**
   * Attempts to decode a `Byte` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `Byte` value and the left-over encoded bits, otherwise an error
   */
  override def decode(from: BitVector): Attempt[DecodeResult[Byte]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (byteBits, remaining) = from.splitAt(byteSize)
      Attempt.successful(DecodeResult(byteBits.toByte(signed = true), remaining))
    }

  /**
   * Encodes a `Byte` into a vector of bits.
   * @param value a `Byte` value to encode
   * @return if successful, an encoded vector of bits representing a `Byte`, otherwise an error
   */
  override def encode(value: Byte): Attempt[BitVector] = Attempt.successful(BitVector(value))

  override def sizeBound: SizeBound = SizeBound.exact(byteSize)
}
