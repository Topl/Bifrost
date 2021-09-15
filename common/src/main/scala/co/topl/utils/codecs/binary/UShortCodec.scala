package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object UShortCodec {

  val minValue: Int = 0
  val maxValue: Int = 0xffff

  /**
   * Attempts to decode a `UShort` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `UShort` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[UShort]] =
    ULongCodec
      .decode(from)
      .flatMap(result =>
        if (result.value >= minValue && result.value <= maxValue)
          Attempt.successful(result.map(_.toInt))
        else
          Attempt.failure(Err("UShort value is outside of valid range."))
      )

  /**
   * Attempts to encode a `UShort` value to a vector of bits.
   * @param value the `UShort` to encode
   * @return if successful, a bit vector of encoded data, otherwise an error
   */
  def encode(value: UShort): Attempt[BitVector] = ULongCodec.encode(value)

  /**
   * `Codec` type-class for encoding/decoding a `UShort` value to/from a bit vector.
   */
  val codec = new Codec[UShort] {
    override def encode(value: UShort): Attempt[BitVector] = UShortCodec.encode(value)

    override def sizeBound: SizeBound = ULongCodec.codec.sizeBound

    override def decode(bits: BitVector): Attempt[DecodeResult[UShort]] = UShortCodec.decode(bits)
  }

  trait Codecs {
    val uShort: Codec[UShort] = codec
  }

  trait Implicits {
    implicit val uShortImplicitCodec: Codec[UShort] = codec
  }

}
