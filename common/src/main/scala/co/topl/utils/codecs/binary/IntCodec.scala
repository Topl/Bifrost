package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagInt, encodeZigZagInt}
import scodec.{Attempt, DecodeResult, Decoder, Encoder, SizeBound}
import scodec.bits.BitVector

object IntCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Int]] =
    ULongCodec.decode(from).map(result => result.map(uLongVal => decodeZigZagInt(uLongVal.toInt)))

  def encode(value: Int): Attempt[BitVector] = ULongCodec.encode(encodeZigZagInt(value))

  trait Implicits {
    implicit val intDecoder: Decoder[Int] = decode

    implicit val intEncoder: Encoder[Int] = new Encoder[Int] {
      override def encode(value: Int): Attempt[BitVector] = IntCodec.encode(value)

      override def sizeBound: SizeBound = SizeBound.atMost(64)
    }
  }
}
