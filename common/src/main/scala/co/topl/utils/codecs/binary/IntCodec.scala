package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagInt, encodeZigZagInt}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

object IntCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Int]] =
    ULongCodec.decode(from).map(result => result.map(uLongVal => decodeZigZagInt(uLongVal.toInt)))

  def encode(value: Int): Attempt[BitVector] = ULongCodec.encode(encodeZigZagInt(value))

  val codec: Codec[Int] = new Codec[UShort] {
    override def decode(bits: BitVector): Attempt[DecodeResult[UShort]] = IntCodec.decode(bits)

    override def encode(value: UShort): Attempt[BitVector] = IntCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atMost(32)
  }

  trait Codecs {
    val int: Codec[Int] = codec
  }

  trait Implicits {
    implicit val intImplicitCodec: Codec[Int] = codec
  }
}
