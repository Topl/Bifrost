package co.topl.utils.codecs.binary.valuetypes

import cats.implicits._
import scodec.bits.BitVector
import scodec.{Attempt, Codec, DecodeResult, Decoder, Encoder, Err, SizeBound}

object OptionCodec {

  private val noneBitVector: BitVector = BitVector(0)

  private val someBitVector: BitVector = BitVector(1)

  /**
   * Attempts to decode an `Option[T]` value from a bit vector.
   * @param from the bit vector of encoded data
   * @tparam T the wrapped type in the `Option` value with an associated `Decoder` type-class instance
   * @return if successful, an `Option[T]` and the left-over bits, otherwise an error
   */
  def decode[T: Decoder](from: BitVector): Attempt[DecodeResult[Option[T]]] =
    if (from.length < byteSize) Attempt.failure(Err.insufficientBits(byteSize, from.length))
    else {
      val (byteBits, remaining) = from.splitAt(byteSize)

      if (byteBits === noneBitVector) Attempt.successful(DecodeResult(None, remaining))
      else Decoder[T].decode(remaining).map(_.map(_.some))
    }

  /**
   * Attempts to encode an `Option[T]` value as a bit vector.
   * @param value the option value to encode
   * @tparam T the wrapped type in the `Option` value with an associated `Encoder` type-class instance
   * @return if successful, a bit vector representing an `Option[T]`, otherwise an error
   */
  def encode[T: Encoder](value: Option[T]): Attempt[BitVector] =
    value match {
      case Some(v) => Encoder[T].encode(v).map(someBitVector ++ _)
      case None    => Attempt.successful(noneBitVector)
    }

  /**
   * Creates a `Codec` type-class instance for `Option[T]` to encode/decode from a bit vector.
   * @tparam T the wrapped type of the option with an associated `Codec` type-class instance
   * @return a `Codec` instance for encoding/decoding an `Option[T]`
   */
  def codec[T: Codec]: Codec[Option[T]] = new Codec[Option[T]] {
    override def decode(bits: BitVector): Attempt[DecodeResult[Option[T]]] = OptionCodec.decode(bits)

    override def encode(value: Option[T]): Attempt[BitVector] = OptionCodec.encode(value)

    override def sizeBound: SizeBound = Codec[T].sizeBound + SizeBound.exact(byteSize)
  }

  trait Codecs {
    def option[T: Codec]: Codec[Option[T]] = OptionCodec.codec
  }

  trait Implicits {
    implicit def optionImplicitCodec[T: Codec]: Codec[Option[T]] = OptionCodec.codec
  }

  object codecs extends Codecs
  object implicits extends Implicits
}
