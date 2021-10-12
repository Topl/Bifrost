package co.topl.utils.codecs.binary.valuetypes

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object ByteCodec {

  /**
   * Attempts to decode a `Byte` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `Byte` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[Byte]] =
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
  def encode(value: Byte): Attempt[BitVector] = Attempt.successful(BitVector(value))

  /**
   * Codec type-class instance for encoding and decoding a `Byte` to/from a bit vector.
   */
  val codec: Codec[Byte] = new Codec[Byte] {
    override def encode(value: Byte): Attempt[BitVector] = ByteCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(byteSize)

    override def decode(bits: BitVector): Attempt[DecodeResult[Byte]] = ByteCodec.decode(bits)
  }

  trait Codecs {
    val byte: Codec[Byte] = codec
  }

  trait Implicits {
    implicit val byteImplicitCodec: Codec[Byte] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}
