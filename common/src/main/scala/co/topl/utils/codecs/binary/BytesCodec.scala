package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object BytesCodec {

  def decode(from: BitVector, size: Int): Attempt[DecodeResult[Array[Byte]]] = {
    val requiredBits = size * byteSize
    if (from.length < requiredBits) Attempt.failure(Err.insufficientBits(requiredBits, from.length))
    else {
      val (decodedBits, remaining) = from.splitAt(requiredBits)

      Attempt.successful(DecodeResult(decodedBits.toByteArray, remaining))
    }
  }

  def encode(value: Array[Byte]): Attempt[BitVector] = Attempt.successful(BitVector(value))

  def codec(size: Int): Codec[Array[Byte]] = new Codec[Array[Byte]] {
    override def encode(value: Array[Byte]): Attempt[BitVector] = BytesCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(size * byteSize)

    override def decode(bits: BitVector): Attempt[DecodeResult[Array[Byte]]] = BytesCodec.decode(bits, size)
  }

  trait Codecs {
    def bytes(size: Int): Codec[Array[Byte]] = codec(size)
  }
}
