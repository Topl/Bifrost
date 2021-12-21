package co.topl.codecs.binary.typeclasses

import akka.util.ByteString
import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits._
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
 * @tparam T the type this type-class is implemented for
 */
@typeclass trait Transmittable[T] {

  /**
   * Encodes a value into its transmittable bytes representation to be sent to another blockchain node.
   * @param value the value to encode into transmittable data
   * @return the data to transmit to another node
   */
  def transmittableBytes(value: T): Array[Byte]

  /**
   * Encodes a value into its transmittable byte-string representation to be sent to another blockchain node.
   * @param value the value to encode into transmittable data
   * @return the data to transmit to another node
   */
  def transmittableByteString(value: T): ByteString = ByteString(transmittableBytes(value))

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
   * Attempts to decode a value from its transmitted byte-string representation into its expected data type.
   *
   * The byte representation should have been transmitted from another node which encoded the value with the same
   * scheme.
   *
   * @param byteString the transmitted byte-string to decode into a value `T`
   * @return if successful, a value of type `T` represented by the transmitted bytes, otherwise a failure message
   */
  def fromTransmittableByteString(byteString: ByteString): Either[String, T]
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

    override def fromTransmittableByteString(byteString: ByteString): Either[String, T] =
      Codec[T].decodeValue(BitVector(byteString)).toEither.leftMap(_.message)
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
   * Extension operations for working with transmitted data in the form of an Akka Byte String
   * @param value the transmitted byte string value
   */
  class ByteStringTransmittableOps(val value: ByteString) extends AnyVal {

    /**
     * Attempts to decode byte data transmitted from another blockchain node into a value of type `T`.
     * @tparam T the type of value to try and decode the byte data to.
     * @return if successful, the value which the transmitted byte data represents, otherwise a failure message
     */
    def decodeTransmitted[T: Transmittable]: Either[String, T] = Transmittable[T].fromTransmittableByteString(value)
  }

  trait ToExtensionOps {
    implicit def toTransmittableBytesOps(value: Array[Byte]): BytesTransmittableOps = new BytesTransmittableOps(value)

    implicit def toTransmittableByteStringOps(value: ByteString): ByteStringTransmittableOps =
      new ByteStringTransmittableOps(value)
  }
}
