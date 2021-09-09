package co.topl.utils.codecs.binary

import co.topl.utils.Int128
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder, Err}

object Int128Codec {

  private val int128BitSize = Int128.numBytes * byteSize

  def decode(from: BitVector): Attempt[DecodeResult[Int128]] =
    if (from.length < int128BitSize)
      Attempt.failure(Err.insufficientBits(int128BitSize, from.length))
    else {
      val (int128Bits, remainder) = from.splitAt(int128BitSize)
      Attempt.successful(DecodeResult(Int128(int128Bits.toByteArray), remainder))
    }

  trait Implicits {
    implicit val int128Decoder: Decoder[Int128] = decode
  }
}
