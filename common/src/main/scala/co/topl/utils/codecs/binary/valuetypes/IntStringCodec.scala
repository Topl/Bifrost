package co.topl.utils.codecs.binary.valuetypes

import co.topl.utils.Extensions.LongOps
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Err, SizeBound}

object IntStringCodec {

  /**
   * The max number of bytes a `String` can contain.
   */
  val maxBytes: Int = Int.MaxValue

  private val maxBitSize: Long = maxBytes.toLong * byteSize

  /**
   * Attempts to decode a `String` from a vector of bits.
   * @param from a bit vector of encoded data
   * @return if successful, a `String` value and the left-over encoded bits, otherwise an error
   */
  def decode(from: BitVector): Attempt[DecodeResult[IntString]] =
    UIntCodec.decode(from).flatMap { result =>
      val stringBitSize = result.value.toIntExact * byteSize

      if (result.remainder.length < stringBitSize)
        Attempt.failure(Err.insufficientBits(stringBitSize, result.remainder.length))
      else {
        val (stringBits, remaining) = result.remainder.splitAt(stringBitSize)

        val stringBytes = stringBits.toByteArray
        val stringBytesLength = stringBytes.length

        if (stringBytesLength <= maxBytes)
          Attempt.successful(DecodeResult(new String(stringBytes, stringCharacterSet), remaining))
        else
          Attempt.failure(Err("IntString value is outside of valid range."))
      }
    }

  /**
   * Attempts to encode a `String` into a vector of bits.
   * @param value the `String` value to encode
   * @return if successful, a vector of encoded bits, otherwise an error
   */
  def encode(value: IntString): Attempt[BitVector] = {
    val byteRepr = value.getBytes(stringCharacterSet)

    val byteLength = byteRepr.length

    if (byteLength <= maxBytes) UIntCodec.encode(byteLength).map(_ ++ BitVector(byteRepr))
    else Attempt.failure(Err("IntString value is outside of valid range."))
  }

  /**
   * Codec type-class instance for encoding/decoding a `String` to/from a bit vector.
   */
  val codec: Codec[IntString] = new Codec[IntString] {
    override def decode(bits: BitVector): Attempt[DecodeResult[IntString]] = IntStringCodec.decode(bits)

    override def encode(value: IntString): Attempt[BitVector] = IntStringCodec.encode(value)

    override def sizeBound: SizeBound = UIntCodec.codec.sizeBound + SizeBound.atMost(maxBitSize)
  }

  trait Codecs {
    val intString: Codec[IntString] = codec
  }

  trait Implicits {
    implicit val intStringImplicitCodec: Codec[IntString] = codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}
