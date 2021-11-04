package co.topl.utils.codecs.binary.scodecs.valuetypes

import co.topl.utils.codecs.binary.scodecs.valuetypes.Constants.byteSize
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

class BytesCodec(size: Int) extends Codec[Array[Byte]] {

  /**
   * Attempts to decode an `Array[Byte]` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, an `Array[Byte]` and the left-over encoded bits, otherwise an error
   */
  override def decode(from: BitVector): Attempt[DecodeResult[Array[Byte]]] = {
    val requiredBits = size * byteSize
    if (from.length < requiredBits) Attempt.failure(Err.insufficientBits(requiredBits, from.length))
    else {
      val (decodedBits, remaining) = from.splitAt(requiredBits)

      Attempt.successful(DecodeResult(decodedBits.toByteArray, remaining))
    }
  }

  /**
   * Encodes an `Array[Byte]` into a vector of bits.
   * @param value an `Array[Byte]` to encode
   * @return if successful, an encoded vector of bits representing an `Array[Byte]`, otherwise an error
   */
  override def encode(value: Array[Byte]): Attempt[BitVector] = Attempt.successful(BitVector(value))

  override def sizeBound: SizeBound = SizeBound.exact(size * byteSize)
}
