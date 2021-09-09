package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagLong
import scodec.{Attempt, DecodeResult, Decoder}
import scodec.bits.BitVector

object LongCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Long]] =
    ULongCodec.decode(from).map(result => result.map(decodeZigZagLong))

  trait Implicits {
    implicit val longDecoder: Decoder[Long] = decode
  }
}
