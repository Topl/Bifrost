package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder, Err}

object ULongCodec {

  private val `0x7f` = BitVector(0x7f)
  private val `0x80` = BitVector(0x80)
  private val `0 bitvector` = BitVector(0)

  def decode(from: BitVector): Attempt[DecodeResult[ULong]] = {
    var result: Long = 0
    var iteration = 0

    while (iteration < 10) {
      val bitPointer = iteration * byteSize

      if (from.length < bitPointer + byteSize)
        return Attempt.failure(Err.insufficientBits(bitPointer + byteSize, from.length))

      val b = from.slice(bitPointer, bitPointer + byteSize)

      result = result | ((b & `0x7f`).toLong() << iteration * 7)

      if ((b & `0x80`) === `0 bitvector`)
        return Attempt.successful(DecodeResult(result, from.drop(bitPointer + byteSize)))

      iteration += 1
    }

    Attempt.failure(Err("Unexpected bytes remaining."))
  }

  trait Implicits {
    implicit def uLongDecoder: Decoder[ULong] = decode
  }

}
