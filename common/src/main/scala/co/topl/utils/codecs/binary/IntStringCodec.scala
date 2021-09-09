package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Extensions.LongOps
import co.topl.utils.UnsignedNumbers.UInt
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder, Encoder, Err, SizeBound}

object IntStringCodec {

  private val maxBitSize: Long = IntString.maxBytes.toLong * byteSize

  def decode(from: BitVector): Attempt[DecodeResult[IntString]] =
    UIntCodec.decode(from).flatMap { result =>
      val stringBitSize = result.value.value.toIntExact * byteSize

      if (result.remainder.length < stringBitSize)
        Attempt.failure(Err.insufficientBits(stringBitSize, result.remainder.length))
      else {
        val (stringBits, remaining) = result.remainder.splitAt(stringBitSize)

        Attempt.fromEither(
          IntString
            .validated(new String(stringBits.toByteArray, stringCharacterSet))
            .leftMap(failure => Err(failure.toString))
            .map(DecodeResult(_, remaining))
        )
      }
    }

  def encode(value: IntString): Attempt[BitVector] = {
    val byteRepr = value.value.getBytes(stringCharacterSet)

    val byteLength = byteRepr.length

    Attempt
      .fromEither(UInt.validated(byteLength).leftMap(failure => Err(failure.toString)))
      .flatMap(UIntCodec.encode)
      .map(_ ++ BitVector(byteRepr))
  }

  trait Implicits {
    implicit val intStringDecoder: Decoder[IntString] = decode

    implicit val intStringEncoder: Encoder[IntString] = new Encoder[IntString] {
      override def encode(value: IntString): Attempt[BitVector] = IntStringCodec.encode(value)

      override def sizeBound: SizeBound = SizeBound.atMost(maxBitSize)
    }
  }
}
