package co.topl.codecs.bytes.scodecs.valuetypes

import cats.implicits._
import co.topl.codecs.bytes.scodecs.valuetypes.Constants.byteSize
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

class OptionCodec[T: Codec] extends Codec[Option[T]] {

  private val noneBitVector: BitVector = BitVector(0)

  private val someBitVector: BitVector = BitVector(1)

  override def decode(bits: BitVector): Attempt[DecodeResult[Option[T]]] =
    if (bits.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, bits.length))
    else {
      val (byteBits, remaining) = bits.splitAt(byteSize)

      if (byteBits === noneBitVector) Attempt.successful(DecodeResult(None, remaining))
      else Decoder[T].decode(remaining).map(_.map(_.some))
    }

  override def encode(value: Option[T]): Attempt[BitVector] =
    value match {
      case Some(v) => Encoder[T].encode(v).map(someBitVector ++ _)
      case None    => Attempt.successful(noneBitVector)
    }

  override def sizeBound: SizeBound = Codec[T].sizeBound + SizeBound.exact(byteSize)
}
