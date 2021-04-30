package co.topl.crypto

import cats.data.Validated
import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

  trait Digest[T] {
    val size: Int

    def from[B: BytesOf](b: B): T
  }

  object Digest {
    def apply[T: Digest]: Digest[T] = implicitly[Digest[T]]

    implicit val digestDigest32: Digest[Digest32] = new Digest[Digest32] {
      override val size: Int = Digest32.size

      override def from[B: BytesOf](b: B): Digest32 = Digest32(BytesOf[B].get(b))
    }
  }

  @newtype
  case class Digest32(value: Array[Byte])

  object Digest32 {
    val size = 32

    /**
     * Gets a validated Digest guaranteed to be the correct digest size.
     * @param bytes the bytes to convert to a digest
     * @return the digest or an invalid error
     */
    def validated(bytes: Array[Byte]): Validated[InvalidDigestError, Digest32] =
      Validated.cond(bytes.length == size, Digest32(bytes), IncorrectSize)
  }

  @newtype
  case class Digest64(value: Array[Byte])

  object Digest64 {
    val size = 64

    /**
     * Gets a validated Digest guaranteed to be the correct digest size.
     * @param bytes the bytes to convert to a digest
     * @return the digest or an invalid error
     */
    def validated(bytes: Array[Byte]): Validated[InvalidDigestError, Digest64] =
      Validated.cond(bytes.length == size, Digest64(bytes), IncorrectSize)

    implicit val digestDigest64: Digest[Digest64] = new Digest[Digest64] {
      override val size: Int = Digest64.size

      override def from[B: BytesOf](b: B): Digest64 = Digest64(BytesOf[B].get(b))
    }
  }

  sealed trait InvalidDigestError
  case object IncorrectSize extends InvalidDigestError

  trait Hash[T, D] {
    def hash[M: BytesOf](prefix: Option[Byte], messages: M*): D

    def hash[M: BytesOf](prefix: Byte, messages: M*): D = hash[M](Some(prefix), messages: _*)

    def hash[M: BytesOf](message: M)(implicit h: Hash[T, D]): D = hash[M](None, message)
  }

  object Hash {
    def apply[T, D](implicit hash: Hash[T, D]): Hash[T, D] = hash
  }

  def blake2b256[T: BytesOf](value: T): Digest32 = Hash[Blake2b, Digest32].hash(value)

  def sha256[T: BytesOf](value: T): Digest32 = Hash[Sha, Digest32].hash(value)

}
