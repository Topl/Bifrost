package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object BytesCodec {

  /**
   * Attempts to decode an `Array[Byte]` from a vector of bits.
   * @param from a bit vector of encoded data
   * @param size the expected size of the array to decode
   * @return if successful, an `Array[Byte]` and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector, size: Int): Attempt[DecodeResult[Array[Byte]]] = {
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
  def encode(value: Array[Byte]): Attempt[BitVector] = Attempt.successful(BitVector(value))

  /**
   * Codec type-class builder for encoding an array of a specific length
   * @param size the size of the decoded array
   * @return a Codec type-class instance for a fixed length array
   */
  def codec(size: Int): Codec[Array[Byte]] = new Codec[Array[Byte]] {
    override def encode(value: Array[Byte]): Attempt[BitVector] = BytesCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(size * byteSize)

    override def decode(bits: BitVector): Attempt[DecodeResult[Array[Byte]]] = BytesCodec.decode(bits, size)
  }

  trait Codecs {
    def bytes(size: Int): Codec[Array[Byte]] = codec(size)
  }
}
