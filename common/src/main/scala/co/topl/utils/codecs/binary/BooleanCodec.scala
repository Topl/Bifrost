package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder, Err}

object BooleanCodec {

  private val trueByte: Byte = 0x01
  private val trueByteBitVector: BitVector = BitVector(trueByte)

  def decode(from: BitVector): Attempt[DecodeResult[Boolean]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (headByteBits, remainder) = from.splitAt(byteSize)
      Attempt.successful(DecodeResult(headByteBits === trueByteBitVector, remainder))
    }

  trait Implicits {

    implicit val booleanDecoder: Decoder[Boolean] = decode

  }
}
