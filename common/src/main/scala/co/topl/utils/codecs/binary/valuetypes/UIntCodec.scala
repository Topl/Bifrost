package co.topl.utils.codecs.binary.valuetypes

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object UIntCodec {

  val minValue: Long = 0
  val maxValue: Long = 0xffffffffL

  /**
   * Attempts to decode a `UInt` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `UInt` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[UInt]] =
    ULongCodec
      .decode(from)
      .flatMap(result =>
        if (result.value >= minValue && result.value <= maxValue) Attempt.successful(result)
        else Attempt.failure(Err("UInt value is outside of valid range."))
      )

  /**
   * Attempts to encode a `UInt` value to a vector of bits.
   * @param value the `UInt` to encode
   * @return if successful, a bit vector of encoded data, otherwise an error
   */
  def encode(value: UInt): Attempt[BitVector] = ULongCodec.encode(value)

  /**
   * `Codec` type-class for encoding/decoding a `UInt` value to/from a bit vector.
   */
  val codec = new Codec[UInt] {
    override def decode(bits: BitVector): Attempt[DecodeResult[UInt]] = UIntCodec.decode(bits)

    override def encode(value: UInt): Attempt[BitVector] = UIntCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atMost(32)
  }

  trait Codecs {

    val uInt: Codec[UInt] = codec
  }

  trait Implicits {

    implicit val uIntImplicitCodec: Codec[UInt] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}
