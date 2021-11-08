package co.topl.codecs.binary.typeclasses

import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.StringDataTypes.Base58Data
import scodec.Codec
import scodec.bits.BitVector
import simulacrum.typeclass

import scala.language.implicitConversions

@typeclass
trait Persistable[T] {

  def persistedBytes(value: T): Array[Byte]

  def fromPersistedBytes(bytes: Array[Byte]): Either[String, T]

  def persistedBase58(value: T): Base58Data = Base58Data.fromData(persistedBytes(value))
}

object Persistable {

  def fromCodec[T: Codec]: Persistable[T] = new Persistable[T] {

    override def persistedBytes(value: T): Array[Byte] =
      Codec[T].encode(value).toEither.getOrThrow().toByteArray

    override def fromPersistedBytes(bytes: Array[Byte]): Either[String, T] =
      Codec[T].decodeValue(BitVector(bytes)).toEither.leftMap(_.message)
  }

  class BytesPersistableOps(val value: Array[Byte]) extends AnyVal {

    def decodePersisted[T: Persistable]: Either[String, T] =
      Persistable[T].fromPersistedBytes(value)
  }

  trait ToExtensionOps {
    implicit def toPersistableBytesOps(value: Array[Byte]): BytesPersistableOps = new BytesPersistableOps(value)
  }
}
