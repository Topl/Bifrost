package co.topl.utils.codecs.binary

import co.topl.utils.serialization.ZigZagEncoder.decodeZigZagInt
import scodec.{Attempt, DecodeResult, Decoder}
import scodec.bits.BitVector

object ShortCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Short]] =
    ULongCodec
      .decode(from)
      .map(result => result.map(uLongValue => decodeZigZagInt(uLongValue.toInt).toShort))

  trait Implicits {
    implicit def shortDecoder: Decoder[Short] = decode
  }
}
