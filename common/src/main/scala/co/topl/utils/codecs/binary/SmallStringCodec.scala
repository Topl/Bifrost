package co.topl.utils.codecs.binary

import cats.implicits._
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Err, SizeBound}

object SmallStringCodec {

  val maxBytes: Int = 255

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
        stringBytes = stringSplitTuple._1.toByteArray
        // run validation on parsed string
        smallString <- Either.cond(
          stringBytes.length <= maxBytes,
          new String(stringBytes, stringCharacterSet),
          Err("SmallString value is outside of valid range.")
        )
      } yield DecodeResult(smallString, stringSplitTuple._2)
    )

  def encode(value: SmallString): Attempt[BitVector] = {
    val byteRepr = value.getBytes(stringCharacterSet)

    val byteLength = byteRepr.length

    if (byteLength <= maxBytes) UIntCodec.encode(byteLength).map(_ ++ BitVector(byteRepr))
    else Attempt.failure(Err("SmallString value is outside of valid range."))
  }

  val codec: Codec[SmallString] = new Codec[SmallString] {
    override def decode(bits: BitVector): Attempt[DecodeResult[SmallString]] = SmallStringCodec.decode(bits)

    override def encode(value: SmallString): Attempt[BitVector] = SmallStringCodec.encode(value)

    override def sizeBound: SizeBound = UIntCodec.codec.sizeBound + SizeBound.atMost(maxBytes * byteSize)
  }

  trait Codecs {
    val smallString: Codec[SmallString] = codec
  }

  trait Implicits {
    implicit val smallStringImplicitCodec: Codec[SmallString] = codec
  }
}
