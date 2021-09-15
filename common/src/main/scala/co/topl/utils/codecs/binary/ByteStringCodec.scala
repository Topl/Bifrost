package co.topl.utils.codecs.binary

import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object ByteStringCodec {

  val maxBytes: Int = 255

  /**
   * Attempts to decode a `String` with max length 255 from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `String` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[ByteString]] =
    Attempt.fromEither(
      for {
        // split input by size bits and remaining
        sizeSplitTuple <-
          Either.cond(from.length >= byteSize, from.splitAt(byteSize), Err.insufficientBits(byteSize, from.length))
        stringSizeBits = sizeSplitTuple._1.toInt(signed = false) * byteSize
        // split remaining from size split by string and remaining
        stringSplitTuple <-
          Either.cond(
            sizeSplitTuple._2.length >= stringSizeBits,
            sizeSplitTuple._2.splitAt(stringSizeBits),
            Err.insufficientBits(stringSizeBits, sizeSplitTuple._2.length)
          )
        stringBytes = stringSplitTuple._1.toByteArray
        // run length validation on parsed string
        smallString <- Either.cond(
          stringBytes.length <= maxBytes,
          new String(stringBytes, stringCharacterSet),
          Err("SmallString value is outside of valid range.")
        )
      } yield DecodeResult(smallString, stringSplitTuple._2)
    )

  /**
   * Attempts to encode a `String` with max length 255 into a vector of bits.
   * @param value the `String` value to encode
   * @return if successful, a vector of encoded bits, otherwise an error
   */
  def encode(value: ByteString): Attempt[BitVector] = {
    val byteRepr = value.getBytes(stringCharacterSet)

    val byteLength = byteRepr.length

    if (byteLength <= maxBytes) UIntCodec.encode(byteLength).map(_ ++ BitVector(byteRepr))
    else Attempt.failure(Err("SmallString value is outside of valid range."))
  }

  /**
   * Codec type-class instance for encoding/decoding a `String` with a maximum length of 255.
   */
  val codec: Codec[ByteString] = new Codec[ByteString] {
    override def decode(bits: BitVector): Attempt[DecodeResult[ByteString]] = ByteStringCodec.decode(bits)

    override def encode(value: ByteString): Attempt[BitVector] = ByteStringCodec.encode(value)

    override def sizeBound: SizeBound = UIntCodec.codec.sizeBound + SizeBound.atMost(maxBytes * byteSize)
  }

  trait Codecs {
    val smallString: Codec[ByteString] = codec
  }

  trait Implicits {
    implicit val smallStringImplicitCodec: Codec[ByteString] = codec
  }
}
