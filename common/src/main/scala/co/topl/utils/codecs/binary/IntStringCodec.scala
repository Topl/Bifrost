package co.topl.utils.codecs.binary

import cats.implicits._
import co.topl.utils.Extensions.LongOps
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder, Err}

object IntStringCodec {

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

  trait Implicits {
    implicit val intStringDecoder: Decoder[IntString] = decode
  }
}
