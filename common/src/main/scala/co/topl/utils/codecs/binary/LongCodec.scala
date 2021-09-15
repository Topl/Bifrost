package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagLong, encodeZigZagLong}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

object LongCodec {

  /**
   * Attempts to decode a `Long` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `Long` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[Long]] =
    ULongCodec.decode(from).map(result => result.map(decodeZigZagLong))

  /**
   * Encodes a `Long` into a vector of bits.
   * @param value a `Long` value to encode
   * @return if successful, an encoded vector of bits representing a `Long`, otherwise an error
   */
  def encode(value: Long): Attempt[BitVector] =
    ULongCodec.encode(encodeZigZagLong(value))

  /**
   * Codec type-class instance to encode/decode a `Long` to/from a bit vector.
   */
  val codec: Codec[Long] = new Codec[Long] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = LongCodec.decode(bits)

    override def encode(value: Long): Attempt[BitVector] = LongCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atMost(64)
  }

  trait Codecs {
    val long: Codec[Long] = codec
  }

  trait Implicits {
    implicit val longImplicitCodec: Codec[Long] = codec
  }
}
