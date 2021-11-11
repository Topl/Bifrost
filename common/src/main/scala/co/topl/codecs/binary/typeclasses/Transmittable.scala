package co.topl.codecs.binary.typeclasses

import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data}
import scodec.Codec
import scodec.bits.BitVector
import simulacrum.typeclass

import scala.language.implicitConversions

/**
 * Typeclass for encoding a value into a byte representation which should be communicated over the network to other
 * blockchain nodes.
 *
 * IMPORTANT:
 * The typeclass instance's encoding scheme should never change so as to not break compatibility between nodes.
 *
 * See `Persistable` or `BinaryShow` for alternative use cases of generating binary data.
 *
 * @tparam T
 */
@typeclass trait Transmittable[T] {

  /**
   * Encodes a value into its transmittable bytes representation to be sent to another blockchain node.
   * @param value the value to encode into transmittable data
   * @return the data to transmit to another node
   */
  def transmittableBytes(value: T): Array[Byte]

  /**
   * Attempts to decode a value from its transmitted bytes representation into its expected data type.
   *
   * The byte representation should have been transmitted from another node which encoded the value with the same
   * scheme.
   *
   * @param bytes the transmitted bytes to decode into a value `T`
   * @return if successful, a value of type `T` represented by the transmitted bytes, otherwise a failure message
   */
  def fromTransmittableBytes(bytes: Array[Byte]): Either[String, T]

  /**
   * Encodes a value into a base-58 encoding of the bytes data to transmit to another blockchain node.
   * @param value the value to encode into a transmittable Base-58 data representation
   * @return the encoded base-58 data representing the value
   */
  def transmittableBase58(value: T): Base58Data = Base58Data.fromData(transmittableBytes(value))

  /**
   * Encodes a value into a base-16 encoding of the bytes data to transmit to another blockchain node.
   * @param value the value to encode into a transmittable Base-16 data representation
   * @return the encoded base-16 data representing the value
   */
  def transmittableBase16(value: T): Base16Data = Base16Data.fromData(transmittableBytes(value))
}

object Transmittable {

  /**
   * Generates an instance of the `Transmittable` typeclass from an instance of the Scodec `Codec` typeclass.
   * @tparam T the type of value that can be transmitted with an existing `Codec` instance
   * @return an instance of the `Transmittable` typeclass for value `T`
   */
  def fromCodec[T: Codec]: Transmittable[T] = new Transmittable[T] {

    override def transmittableBytes(value: T): Array[Byte] =
      Codec[T].encode(value).getOrThrow().toByteArray

    override def fromTransmittableBytes(bytes: Array[Byte]): Either[String, T] =
      Codec[T].decodeValue(BitVector(bytes)).toEither.leftMap(_.message)
  }

  /**
   * Extension operations for working with transmitted byte data.
   * @param value the transmitted byte data to operate on
   */
  class BytesTransmittableOps(val value: Array[Byte]) extends AnyVal {

    /**
     * Attempts to decode byte data transmitted from another blockchain node into a value of type `T`.
     * @tparam T the type of value to try and decode the byte data to.
     * @return if successful, the value which the transmitted byte data represents, otherwise a failure message
     */
    def decodeTransmitted[T: Transmittable]: Either[String, T] = Transmittable[T].fromTransmittableBytes(value)
  }

  /**
   * Extension operations for working with transmitted byte data encoded in Base-58.
   * @param value the transmitted Base-58 encoded data to operate on
   */
  class Base58TransmittableOps(val value: Base58Data) extends AnyVal {

    /**
     * Attempts to decode base-58 encoded data transmitted from another blockchain node into a value of type `T`.
     * @tparam T the type of value to try and decode the base-58 encoded to.
     * @return if successful, the value which the transmitted base-58 data represents, otherwise a failure message
     */
    def decodeTransmitted[T: Transmittable]: Either[String, T] = Transmittable[T].fromTransmittableBytes(value.value)
  }

  trait ToExtensionOps {
    implicit def toTransmittableBytesOps(value: Array[Byte]): BytesTransmittableOps = new BytesTransmittableOps(value)

    implicit def toTransmittableBase58Ops(value: Base58Data): Base58TransmittableOps = new Base58TransmittableOps(value)
  }
}
