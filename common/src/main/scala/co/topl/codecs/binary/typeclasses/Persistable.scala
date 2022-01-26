package co.topl.codecs.binary.typeclasses

import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Base58Data
import scodec.Codec
import scodec.bits.BitVector
import simulacrum.typeclass

import scala.language.implicitConversions

/**
 * Typeclass for encoding a value into its byte representation for persisting to a data store.
 * This byte representation has flexibility to be changed without requiring any forks of the network.
 *
 * See `Transmittable` for a byte-representation that should be immutable.
 *
 * @tparam T the value that this typeclass is defined for
 */
@typeclass trait Persistable[T] {

  /**
   * Gets the byte representation of the value that should be persisted to a data store.
   * @param value the value to convert into bytes
   * @return an array of bytes representing the value which should then be persisted as-is
   */
  def persistedBytes(value: T): Array[Byte]

  /**
   * Attempts to decode a value of type `T` from a given array of bytes.
   * The given bytes should have been generated using the `persistedBytes` function.
   * @param bytes the persisted bytes to attempt to decode into a value of `T`
   * @return if successful, a value of type `T` represented by the input bytes, otherwise a failure message
   */
  def fromPersistedBytes(bytes: Array[Byte]): Either[String, T]

  /**
   * Encodes the value with its base-58 representation.
   * This function should primarily be used for debug purposes.
   * @param value the value to encode into base-58 data
   * @return the base-58 representation of the input value
   */
  def persistedBase58(value: T): Base58Data = Base58Data.fromData(persistedBytes(value))
}

object Persistable {

  /**
   * Generates an instance of the `Persistable` typeclass from an instance of the Scodec `Codec` typeclass.
   * @tparam T the type of value that can be persisted with an existing `Codec` instance
   * @return an instance of the `Persistable` typeclass for value `T`
   */
  def fromCodec[T: Codec]: Persistable[T] = new Persistable[T] {

    override def persistedBytes(value: T): Array[Byte] =
      Codec[T].encode(value).toEither.getOrThrow().toByteArray

    override def fromPersistedBytes(bytes: Array[Byte]): Either[String, T] =
      Codec[T].decodeValue(BitVector(bytes)).toEither.leftMap(_.message)
  }

  /**
   * Extension operations for working with persisted byte data.
   * @param value the persisted byte data to operate on
   */
  class BytesPersistableOps(val value: Array[Byte]) extends AnyVal {

    /**
     * Attempts to decode persisted byte data into a value of type `T`.
     * The byte data should come from a persisted data store and have been generated using the
     * `Persistable.persistedBytes` typeclass function.
     * @tparam T the type of value to attempt to decode to
     * @return if successful, the value of type `T` that the data represents, otherwise a failure message
     */
    def decodePersisted[T: Persistable]: Either[String, T] =
      Persistable[T].fromPersistedBytes(value)
  }

  trait ToExtensionOps {
    implicit def toPersistableBytesOps(value: Array[Byte]): BytesPersistableOps = new BytesPersistableOps(value)
  }
}
