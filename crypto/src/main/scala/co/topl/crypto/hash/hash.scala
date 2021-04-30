package co.topl.crypto

import cats.data.Validated
import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

  trait Digest[T] {
    val size: Int

    def from(b: Array[Byte]): T

    def bytes(d: T): Array[Byte]

    def concat[B: Digest](d: T, b: B): Array[Byte] = bytes(d) ++ Digest[B].bytes(b)

    def sameElements[B: Digest](d: T, b: B): Boolean = bytes(d) sameElements Digest[B].bytes(b)
  }

  object Digest {
    def apply[T: Digest]: Digest[T] = implicitly[Digest[T]]
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

    implicit val digestDigest32: Digest[Digest32] = new Digest[Digest32] {
      override val size: Int = Digest32.size

      override def from(b: Array[Byte]): Digest32 = Digest32(b)

      override def bytes(d: Digest32): Array[Byte] = d.value
    }
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

      override def from(b: Array[Byte]): Digest64 = Digest64(b)

      override def bytes(d: Digest64): Array[Byte] = d.value
    }
  }

  sealed trait InvalidDigestError
  case object IncorrectSize extends InvalidDigestError

  abstract class Hash[H, D: Digest] {

    def hash(prefix: Option[Byte], messages: Array[Byte]*): D

    def hash(prefix: Byte, messages: Array[Byte]*): D = hash(Some(prefix), messages: _*)

    def hash(message: Array[Byte])(implicit h: Hash[H, D]): D = hash(None, message)
  }

  object Hash {
    def apply[H, D: Digest](implicit hash: Hash[H, D]): Hash[H, D] = hash
  }

  def blake2b256(value: Array[Byte]): Digest32 = Hash[Blake2b, Digest32].hash(value)

  def blake2b512(value: Array[Byte]): Digest64 = Hash[Blake2b, Digest64].hash(value)

  def sha256(value: Array[Byte]): Digest32 = Hash[Sha, Digest32].hash(value)

  def sha512(value: Array[Byte]): Digest64 = Hash[Sha, Digest64].hash(value)

}
