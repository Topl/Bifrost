package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object UShortCodec {

  val minValue: Int = 0
  val maxValue: Int = 0xffff

  def decode(from: BitVector): Attempt[DecodeResult[UShort]] =
    ULongCodec
      .decode(from)
      .flatMap(result =>
        if (result.value >= minValue && result.value <= maxValue)
          Attempt.successful(result.map(_.toInt))
        else
          Attempt.failure(Err("UShort value is outside of valid range."))
      )

  def encode(value: UShort): Attempt[BitVector] = ULongCodec.encode(value)

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
