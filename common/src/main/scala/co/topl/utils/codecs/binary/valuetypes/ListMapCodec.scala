package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.codecs.binary.valuetypes.codecs._
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, SizeBound}
import scodec.bits.BitVector

import scala.collection.immutable.ListMap

object ListMapCodec {

  def encode[A: Encoder, B: Encoder](value: ListMap[A, B]): Attempt[BitVector] =
    for {
      encodedSize <- uInt.encode(value.size)
      encodedResult <- value.foldLeft(Attempt.successful(encodedSize)) { case (bitsResult, (k, v)) =>
        for {
          currentBits <- bitsResult
          keyBits     <- Encoder[A].encode(k)
          valueBits   <- Encoder[B].encode(v)
        } yield currentBits ++ keyBits ++ valueBits
      }
    } yield encodedResult

  def decode[A: Decoder, B: Decoder](from: BitVector): Attempt[DecodeResult[ListMap[A, B]]] =
    for {
      sizeDecodeResult <- uInt.decode(from)
      sizeValue = sizeDecodeResult.value
      sizeRemaining = sizeDecodeResult.remainder
      keyValuePairs <-
        (0L until sizeValue).foldLeft(Attempt.successful(DecodeResult(ListMap[A, B](), sizeRemaining))) {
          case (result, _) =>
            for {
              currentResult <- result
              currentRemainingBits = currentResult.remainder
              currentMap = currentResult.value
              keyDecodeResult <- Decoder[A].decode(currentRemainingBits)
              keyDecodeRemainingBits = keyDecodeResult.remainder
              keyDecodeValue = keyDecodeResult.value
              valueDecodeResult <- Decoder[B].decode(keyDecodeRemainingBits)
              valueDecodeRemainingBits = valueDecodeResult.remainder
              valueDecodeValue = valueDecodeResult.value
            } yield DecodeResult(currentMap.updated(keyDecodeValue, valueDecodeValue), valueDecodeRemainingBits)
        }
    } yield keyValuePairs

  def codec[A: Codec, B: Codec]: Codec[ListMap[A, B]] = new Codec[ListMap[A, B]] {
    override def encode(value: ListMap[A, B]): Attempt[BitVector] = ListMapCodec.encode(value)

    override def sizeBound: SizeBound = SizeBound.atLeast(uInt.sizeBound.lowerBound)

    override def decode(bits: BitVector): Attempt[DecodeResult[ListMap[A, B]]] = ListMapCodec.decode(bits)
  }

  trait Codecs {
    def listMap[A: Codec, B: Codec]: Codec[ListMap[A, B]] = codec
  }

  trait Implicits {
    implicit def implicitListMap[A: Codec, B: Codec]: Codec[ListMap[A, B]] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits

}
