package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagInt, encodeZigZagInt}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

object ShortCodec {

  /**
   * Attempts to decode a `Short` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `Short` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[Short]] =
    ULongCodec
      .decode(from)
      .map(result => result.map(uLongValue => decodeZigZagInt(uLongValue.toInt).toShort))

  /**
   * Encodes a `Short` into a vector of bits.
   * @param value a `Short` value to encode
   * @return if successful, an encoded vector of bits representing a `Long`, otherwise an error
   */
  def encode(value: Short): Attempt[BitVector] = ULongCodec.encode(encodeZigZagInt(value))

  /**
   * Codec type-class for encoding/decoding a `Short` value to/from a bit vector.
   */
  val codec: Codec[Short] = new Codec[Short] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Short]] = ShortCodec.decode(bits)

    override def encode(value: Short): Attempt[BitVector] = ShortCodec.encode(value)

    override def sizeBound: SizeBound = ULongCodec.codec.sizeBound
  }

  trait Codecs {
    implicit val shortCodec: Codec[Short] = codec
  }

  object codecs extends Codecs
}
