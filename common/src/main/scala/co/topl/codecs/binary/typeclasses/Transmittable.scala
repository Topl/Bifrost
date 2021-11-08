package co.topl.codecs.binary.typeclasses

import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits._
import co.topl.utils.StringDataTypes.{Base16Data, Base58Data}
import scodec.Codec
import scodec.bits.BitVector
import simulacrum.typeclass

import scala.language.implicitConversions

@typeclass
trait Transmittable[T] {

  def transmittableBytes(value: T): Array[Byte]

  def fromTransmittableBytes(bytes: Array[Byte]): Either[String, T]

  def transmittableBase58(value: T): Base58Data = Base58Data.fromData(transmittableBytes(value))

  def transmittableBase16(value: T): Base16Data = Base16Data.fromData(transmittableBytes(value))
}

object Transmittable {

  def fromCodec[T: Codec]: Transmittable[T] = new Transmittable[T] {

    override def transmittableBytes(value: T): Array[Byte] =
      Codec[T].encode(value).getOrThrow().toByteArray

    override def fromTransmittableBytes(bytes: Array[Byte]): Either[String, T] =
      Codec[T].decodeValue(BitVector(bytes)).toEither.leftMap(_.message)
  }

  class BytesTransmittableOps(val value: Array[Byte]) extends AnyVal {
    def decodeTransmitted[T: Transmittable]: Either[String, T] = Transmittable[T].fromTransmittableBytes(value)
  }

  class Base58TransmittableOps(val value: Base58Data) extends AnyVal {
    def decodeTransmitted[T: Transmittable]: Either[String, T] = Transmittable[T].fromTransmittableBytes(value.value)
  }

  trait ToExtensionOps {
    implicit def toTransmittableBytesOps(value: Array[Byte]): BytesTransmittableOps = new BytesTransmittableOps(value)

    implicit def toTransmittableBase58Ops(value: Base58Data): Base58TransmittableOps = new Base58TransmittableOps(value)
  }
}
