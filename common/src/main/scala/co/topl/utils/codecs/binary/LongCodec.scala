package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagLong
import scodec.{Attempt, DecodeResult, Decoder, Encoder, SizeBound}
import scodec.bits.BitVector

object LongCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Long]] =
    ULongCodec.decode(from).map(result => result.map(decodeZigZagLong))

  def encode(value: Long): Attempt[BitVector] =
    ULongCodec.encode(value)

  trait Implicits {
    implicit val longDecoder: Decoder[Long] = decode

    implicit val longEncoder: Encoder[Long] = new Encoder[Long] {
      override def encode(value: ULong): Attempt[BitVector] = LongCodec.encode(value)

      override def sizeBound: SizeBound = SizeBound.atMost(64)
    }
  }
}
