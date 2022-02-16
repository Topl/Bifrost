package co.topl.codecs.bytes.typeclasses

import scodec.{Attempt, Codec, Decoder, Encoder}
import scodec.bits.ByteVector
import simulacrum.{op, typeclass}

import scala.language.implicitConversions
import cats.implicits._

/**
 * Typeclass for encoding a value using a stable encoder/decoder scheme.  This scheme must never changed;
 * new schemes must instead be created.
 *
 * @tparam T the value that this typeclass is defined for
 */
@typeclass trait ImmutableCodec[T] extends ImmutableEncoder[T] with ImmutableDecoder[T]

object ImmutableCodec {

  def fromScodecCodec[T: Codec]: ImmutableCodec[T] =
    new ImmutableCodec[T] {
      private val encoder = ImmutableEncoder.fromScodecEncoder[T]
      private val decoder = ImmutableDecoder.fromScodecDecoder[T]
      def immutableBytes(value: T): ByteVector = encoder.immutableBytes(value)
      def fromImmutableBytes(bytes: ByteVector): Either[String, T] = decoder.fromImmutableBytes(bytes)
    }
}

@typeclass trait ImmutableEncoder[T] {

  /**
   * Gets the byte representation of the value that should be persisted to a data store.
   * @param value the value to convert into bytes
   * @return an array of bytes representing the value which should then be persisted as-is
   */
  @op("immutableBytes") def immutableBytes(value: T): ByteVector
}

object ImmutableEncoder {

  def fromScodecEncoder[T: Encoder]: ImmutableEncoder[T] =
    t =>
      Encoder[T].encode(t) match {
        case Attempt.Successful(value) => value.toByteVector
        case Attempt.Failure(cause)    => throw new IllegalArgumentException(cause.messageWithContext)
      }
}

@typeclass trait ImmutableDecoder[T] {

  /**
   * Attempts to decode a value of type `T` from a given array of bytes.
   * The given bytes should have been generated using the `persistedBytes` function.
   * @param bytes the persisted bytes to attempt to decode into a value of `T`
   * @return if successful, a value of type `T` represented by the input bytes, otherwise a failure message
   */
  def fromImmutableBytes(bytes: ByteVector): Either[String, T]

}

object ImmutableDecoder {

  def fromScodecDecoder[T: Decoder]: ImmutableDecoder[T] =
    t => Decoder[T].decodeValue(t.toBitVector).toEither.leftMap(e => e.messageWithContext)
}
