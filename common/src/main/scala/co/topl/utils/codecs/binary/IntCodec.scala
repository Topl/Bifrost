package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagInt, encodeZigZagInt}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, SizeBound}
import scodec.bits.BitVector

object IntCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Int]] =
    ULongCodec.decode(from).map(result => result.map(uLongVal => decodeZigZagInt(uLongVal.toInt)))

  def encode(value: Int): Attempt[BitVector] = ULongCodec.encode(encodeZigZagInt(value))

  val codec: Codec[Int] = new Codec[UShort] {
    override def decode(bits: BitVector): Attempt[DecodeResult[UShort]] = IntCodec.decode(bits)

    override def encode(value: UShort): Attempt[BitVector] = IntCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atMost(64)
  }

  trait Codecs {
    val int: Codec[Int] = codec
  }

  trait Implicits {
    implicit val intImplicitCodec: Codec[Int] = codec
  }
}
