package co.topl.utils.codecs.binary

import cats.implicits._
import scodec.bits.BitVector
import scodec.{Attempt, DecodeResult, Decoder, Err}

object SmallStringCodec {

  def decode(from: BitVector): Attempt[DecodeResult[SmallString]] =
    Attempt.fromEither(
      for {
        // split input by size bits and remaining
        sizeSplitTuple <-
          Either.cond(from.length < byteSize, from.splitAt(byteSize), Err.insufficientBits(byteSize, from.length))
        stringSizeBits = sizeSplitTuple._1.toInt(signed = false)
        // split remaining from size split by string and remaining
        stringSplitTuple <-
          Either.cond(
            sizeSplitTuple._2.length < stringSizeBits,
            sizeSplitTuple._2.splitAt(stringSizeBits),
            Err.insufficientBits(stringSizeBits, sizeSplitTuple._2.length)
          )
        parsedString = new String(stringSplitTuple._1.toByteArray, stringCharacterSet)
        // run validation on parsed string
        smallString <- SmallString
          .validated(parsedString)
          .leftMap(failure => Err(failure.toString))
          .map(DecodeResult(_, stringSplitTuple._2))
      } yield smallString
    )

  trait Implicits {
    implicit val smallStringDecoder: Decoder[SmallString] = decode
  }
}
