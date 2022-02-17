package co.topl.codecs.bytes.scodecs.valuetypes

import co.topl.codecs.bytes.scodecs.valuetypes.Types.ULong
import co.topl.codecs.bytes.scodecs.valuetypes.varint64fast.{Varint64FastDecode, Varint64FastEncode}
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, SizeBound}

import scala.util.Try

object ULongFastCodec extends Codec[ULong] {

  override def decode(bits: BitVector): Attempt[DecodeResult[ULong]] = {
    val varint64FastDecode = new Varint64FastDecode(bits.toByteArray)

    val result = Attempt.fromTry(Try(varint64FastDecode.readRawVarint64()))

    val remaining = BitVector(varint64FastDecode.getRemaining)

    result.map(DecodeResult(_, remaining))
  }

  override def encode(value: ULong): Attempt[BitVector] = {
    val varint64FastEncode = new Varint64FastEncode

    val result = Attempt.fromTry(Try(varint64FastEncode.writeVarint64(value)))

    val bits = BitVector(varint64FastEncode.getResult)

    result.map(_ => bits)
  }

  override def sizeBound: SizeBound = SizeBound.bounded(8, 64)
}
