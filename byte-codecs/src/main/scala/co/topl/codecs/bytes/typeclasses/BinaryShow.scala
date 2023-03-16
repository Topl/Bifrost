package co.topl.codecs.bytes.typeclasses

import com.google.protobuf.ByteString
import scodec.Attempt
import scodec.Encoder
import simulacrum.op
import simulacrum.typeclass

/**
 * Typeclass for encoding a value into its bytes representation for use with presenting to user or for debugging.
 *
 * The binary encoding implementations for instances of `BinaryShow` may be volatile and change often.
 *
 * Do not use `BinaryShow` for any data that should be persisted to data storage or transmitted
 * to other blockchain nodes. See `Persistable` and `Transmittable` for these use-cases.
 *
 * @tparam T the type that can be encoded into byte data for debugging or presentation
 */
@typeclass trait BinaryShow[T] {

  /**
   * Encodes a value into its byte representation for debugging,
   * presenting to an end user, or in-memory equality checks.
   *
   * IMPORTANT: do not use this function transmitting data to other nodes
   * or persisting data to a data store. Use `Transmittable.transmittableBytes` or `Persistable.persistedBytes`
   * instead.
   *
   * @param value the value to encode into bytes
   * @return the bytes-representation of the value.
   */
  @op("showBytes") def encodeAsBytes(value: T): ByteString

  def map[A](transform: A => T): BinaryShow[A] = (value: A) => encodeAsBytes(transform(value))
}

object BinaryShow {

  /**
   * Implements an instance of `BinaryShow` for a type `T` using the `encode` function from an instance of
   * the Scodec `Codec` typeclass.
   * @tparam T the value to create an instance of `BinaryShow` for
   * @return an instance of `BinaryShow` for type `T`
   */
  def instanceFromEncoder[T: Encoder]: BinaryShow[T] = Encoder[T].encode(_) match {
    case Attempt.Successful(value) => ByteString.copyFrom(value.toByteBuffer)
    case Attempt.Failure(cause)    => throw new IllegalArgumentException(cause.messageWithContext)
  }
}
