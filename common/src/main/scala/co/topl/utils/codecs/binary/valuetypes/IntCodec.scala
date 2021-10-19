package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.ZigZagEncoder.{decodeZigZagInt, encodeZigZagInt}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

object IntCodec {

  /**
   * Attempts to decode an `Int` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, an `Int` and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[Int]] =
    ULongCodec.decode(from).map(result => result.map(uLongVal => decodeZigZagInt(uLongVal.toInt)))

  /**
   * Attempts to encode an `Int` as a vector of bits.
   * @param value the value to encode
   * @return if successful, a vector of encoded bits, otherwise an error
   */
  def encode(value: Int): Attempt[BitVector] = ULongCodec.encode(encodeZigZagInt(value))

  /**
   * Codec type-class instance for encoding/decoding an `Int` value to/from a bit vector.
   */
  val codec: Codec[Int] = new Codec[UShort] {
    override def decode(bits: BitVector): Attempt[DecodeResult[UShort]] = IntCodec.decode(bits)

    override def encode(value: UShort): Attempt[BitVector] = IntCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atMost(32)
  }

  trait Codecs {
    implicit val intCodec: Codec[Int] = codec
  }

  object codecs extends Codecs
}
