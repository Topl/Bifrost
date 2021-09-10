package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.{decodeZigZagLong, encodeZigZagLong}
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, SizeBound}
import scodec.bits.BitVector

object LongCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Long]] =
    ULongCodec.decode(from).map(result => result.map(decodeZigZagLong))

  def encode(value: Long): Attempt[BitVector] =
    ULongCodec.encode(encodeZigZagLong(value))

  val codec: Codec[Long] = new Codec[Long] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Long]] = LongCodec.decode(bits)

    override def encode(value: Long): Attempt[BitVector] = LongCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atMost(64)
  }

  trait Codecs {
    val long: Codec[Long] = codec
  }

  trait Implicits {
    implicit val longImplicitCodec: Codec[Long] = codec
  }
}
