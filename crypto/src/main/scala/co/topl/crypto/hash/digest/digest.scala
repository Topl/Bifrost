package co.topl.crypto.hash

import cats.data.Validated
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._
import simulacrum.typeclass

import scala.language.implicitConversions

package object digest {

  @typeclass
  trait Digest[T] {
    def size: Int

    def from(bytes: Array[Byte]): Validated[InvalidDigestError, T]

    def bytes(d: T): Array[Byte]
  }

  @newtype
  class Digest32(val value: Array[Byte])

  object Digest32 {
    val size = 32

    /**
     * Gets a validated Digest guaranteed to be the correct digest size.
     *
     * @param bytes the bytes to convert to a digest
     * @return the digest or an invalid error
     */
    def validated(bytes: Array[Byte]): Validated[InvalidDigestError, Digest32] =
      Validated.cond(bytes.length == size, bytes.coerce, IncorrectSize)
  }

  @newtype
  class Digest64(val value: Array[Byte])

  object Digest64 {
    val size = 64

    /**
     * Gets a validated Digest guaranteed to be the correct digest size.
     *
     * @param bytes the bytes to convert to a digest
     * @return the digest or an invalid error
     */
    def validated(bytes: Array[Byte]): Validated[InvalidDigestError, Digest64] =
      Validated.cond(bytes.length == size, bytes.coerce, IncorrectSize)
  }

  sealed trait InvalidDigestError
  case object IncorrectSize extends InvalidDigestError

  trait instances {

    implicit val digestDigest32: Digest[Digest32] = new Digest[Digest32] {
      override def size: Int = Digest32.size

      override def from(bytes: Array[Byte]): Validated[InvalidDigestError, Digest32] = Digest32.validated(bytes)

      override def bytes(d: Digest32): Array[Byte] = d.value
    }

    implicit val digestDigest64: Digest[Digest64] = new Digest[Digest64] {
      override def size: Int = Digest64.size

      override def from(bytes: Array[Byte]): Validated[InvalidDigestError, Digest64] = Digest64.validated(bytes)

      override def bytes(d: Digest64): Array[Byte] = d.value
    }
  }

  object implicits extends instances with Digest.ToDigestOps
}
