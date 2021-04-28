package co.topl.crypto

import cats.data.Validated
import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

  trait Digest[T] {
    val size: Int
  }

  object Digest {
    def apply[T: Digest]: Digest[T] = implicitly[Digest[T]]
  }

  @newtype
  case class Digest32(value: Array[Byte])

  object Digest32 {
    val size = 32

    /** Gets a validated Digest guaranteed to be the correct digest size.
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

    /** Gets a validated Digest guaranteed to be the correct digest size.
     * @param bytes the bytes to convert to a digest
     * @return the digest or an invalid error
     */
    def validated(bytes: Array[Byte]): Validated[InvalidDigestError, Digest64] =
      Validated.cond(bytes.length == size, Digest64(bytes), IncorrectSize)
  }

  sealed trait InvalidDigestError
  case object IncorrectSize extends InvalidDigestError

  trait Hash[T, D] {
    def hash[M: BytesOf](prefix: Option[Byte], messages: M*): D

    def hash[M: BytesOf](prefix: Byte, messages: M*): D = hash[M](Some(prefix), messages: _*)

    def hash[M: BytesOf](message: M)(implicit h: Hash[T, D]): D = hash[M](None, message)
  }

  object Hash {
    def apply[T, D](implicit hash: Hash[T, D]): Hash[T, D] = implicitly[Hash[T, D]]
  }

}
