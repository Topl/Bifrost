package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

object UIntCodec {

  val minValue: Long = 0
  val maxValue: Long = 0xffffffffL

  def decode(from: BitVector): Attempt[DecodeResult[UInt]] =
    ULongCodec
      .decode(from)
      .flatMap(result =>
        if (result.value >= minValue && result.value <= maxValue) Attempt.successful(result)
        else Attempt.failure(Err("UInt value is outside of valid range."))
      )

  def encode(value: UInt): Attempt[BitVector] = ULongCodec.encode(value)

  val codec = new Codec[UInt] {
    override def decode(bits: BitVector): Attempt[DecodeResult[UInt]] = UIntCodec.decode(bits)

    override def encode(value: UInt): Attempt[BitVector] = UIntCodec.encode(value)

    override def sizeBound: SizeBound = ULongCodec.codec.sizeBound
  }

  trait Codecs {

    val uInt: Codec[UInt] = codec
  }

  trait Implicits {

    implicit val uIntImplicitCodec: Codec[UInt] = codec
  }
}
