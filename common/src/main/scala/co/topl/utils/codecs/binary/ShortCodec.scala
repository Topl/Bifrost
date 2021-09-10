package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagInt, encodeZigZagInt}
import scodec.{Attempt, Codec, DecodeResult, Decoder, SizeBound}
import scodec.bits.BitVector

object ShortCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Short]] =
    ULongCodec
      .decode(from)
      .map(result => result.map(uLongValue => decodeZigZagInt(uLongValue.toInt).toShort))

  def encode(value: Short): Attempt[BitVector] = ULongCodec.encode(encodeZigZagInt(value))

  val codec: Codec[Short] = new Codec[Short] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Short]] = ShortCodec.decode(bits)

    override def encode(value: Short): Attempt[BitVector] = ShortCodec.encode(value)

    override def sizeBound: SizeBound = ULongCodec.codec.sizeBound
  }

  trait Codecs {
    val short: Codec[Short] = codec
  }

  trait Implicits {
    implicit val shortImplicitCodec: Codec[Short] = codec
  }
}
