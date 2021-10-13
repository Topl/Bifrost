package co.topl.utils.codecs.binary.valuetypes

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, SizeBound}

object ListCodec {

  def encode[T: Encoder](value: List[T]): Attempt[BitVector] =
    value.foldLeft(UIntCodec.encode(value.length)) { case (bits, next) =>
      for {
        encodedBits  <- bits
        encodedValue <- Encoder[T].encode(next)
      } yield encodedBits ++ encodedValue
    }

  def decode[T: Decoder](from: BitVector): Attempt[DecodeResult[List[T]]] =
    for {
      uintDecode <- UIntCodec.decode(from)
      listSize = uintDecode.value
      remaining = uintDecode.remainder
      list <- (0L until listSize).foldLeft(Attempt.successful(DecodeResult[List[T]](List(), remaining))) {
        case (result, _) =>
          for {
            currentResult <- result
            currentRemainingBits = currentResult.remainder
            currentList = currentResult.value
            nextDecodeResult <- Decoder[T].decode(currentRemainingBits)
          } yield nextDecodeResult.map(currentList :+ _)
      }
    } yield list

  def codec[T: Codec]: Codec[List[T]] = new Codec[List[T]] {
    override def encode(value: List[T]): Attempt[BitVector] = ListCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atLeast(UIntCodec.codec.sizeBound.lowerBound)

    override def decode(bits: BitVector): Attempt[DecodeResult[List[T]]] = ListCodec.decode(bits)
  }

  trait Codecs {
    implicit def listCodec[T: Codec]: Codec[List[T]] = codec
  }

  object codecs extends Codecs

}
