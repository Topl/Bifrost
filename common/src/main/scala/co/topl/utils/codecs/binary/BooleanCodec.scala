package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object BooleanCodec {

  private val trueByte: Byte = 0x01
  private val trueByteBitVector: BitVector = BitVector(trueByte)

  private val falseByte: Byte = 0x00
  private val falseByteBitVector: BitVector = BitVector(falseByte)

  /**
   * Decodes a `Boolean` value from a vector of bits.
   * @param from the bits to decode a value from
   * @return if successful, a boolean value and the left-over bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[Boolean]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (headByteBits, remainder) = from.splitAt(byteSize)
      Attempt.successful(DecodeResult(headByteBits === trueByteBitVector, remainder))
    }

  /**
   * Encodes a `Boolean` value as a vector of bits.
   * Value is encoded as 0x00 if false and 0x01 if true.
   * @param value the boolean value to encode
   * @return if successful, a vector of encoded bits, otherwise an error
   */
  def encode(value: Boolean): Attempt[BitVector] =
    if (value) Attempt.successful(trueByteBitVector)
    else Attempt.successful(falseByteBitVector)

  /**
   * Codec type-class instance for encoding and decoding a `Boolean` value to/from a bit vector.
   */
  val codec: Codec[Boolean] = new Codec[Boolean] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Boolean]] = BooleanCodec.decode(bits)

    override def encode(value: Boolean): Attempt[BitVector] = BooleanCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(byteSize)
  }

  trait Codecs {
    val bool: Codec[Boolean] = codec
  }

  trait Implicits {
    implicit val booleanImplicitCodec: Codec[Boolean] = codec
  }
}
