package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt
import scodec.{Attempt, DecodeResult, Decoder}
import scodec.bits.BitVector

object IntCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Int]] =
    ULongCodec.decode(from).map(result => result.map(uLongVal => decodeZigZagInt(uLongVal.toInt)))

  trait Implicits {
    implicit val intDecoder: Decoder[Int] = decode
  }
}
