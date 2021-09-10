package co.topl.utils.codecs.binary

import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}
import scodec.bits.BitVector

object ByteCodec {

  def decode(from: BitVector): Attempt[DecodeResult[Byte]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (byteBits, remaining) = from.splitAt(byteSize)
      Attempt.successful(DecodeResult(byteBits.toByte(signed = true), remaining))
    }

  def encode(value: Byte): Attempt[BitVector] = Attempt.successful(BitVector(value))

  val codec: Codec[Byte] = new Codec[Byte] {
    override def encode(value: Byte): Attempt[BitVector] = ByteCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.exact(byteSize)

    override def decode(bits: BitVector): Attempt[DecodeResult[Byte]] = ByteCodec.decode(bits)
  }

  trait Codecs {
    val byte: Codec[Byte] = codec
  }

  trait Implicits {
    implicit val byteImplicitCodec: Codec[Byte] = codec
  }
}
