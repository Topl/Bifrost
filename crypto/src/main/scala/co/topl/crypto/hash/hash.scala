package co.topl.crypto

import cats.data.{NonEmptyChain, Validated}
import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

  @newtype
  case class Digest(toBytes: Array[Byte])

  object Digest {

    def validated(bytes: Array[Byte]): Validated[InvalidDigestError, Digest] =
      Validated.cond(bytes.length == 32, Digest(bytes), IncorrectSize)

  }

  sealed trait InvalidDigestError
  case object IncorrectSize extends InvalidDigestError

  /** A hash function which hashes a message into a digest of type T.
   * @tparam T the digest type
   */
  trait Hash[T] {
    val digestSize: Int
    def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): Digest
  }

  object Hash {

    def apply[T : Hash]: Hash[T] = implicitly[Hash[T]]

    def apply[T : Hash](message: Array[Byte]): Digest =
      apply.hash(None, NonEmptyChain(message))

    def apply[T : Hash](prefix: Byte, messages: NonEmptyChain[Array[Byte]]): Digest =
      apply.hash(Some(prefix), messages)

    def apply[T : Hash](prefix: Byte, message: Array[Byte]): Digest =
      apply.hash(Some(prefix), NonEmptyChain(message))

    def apply[T : Hash](message: String): Digest =
      apply.hash(None, NonEmptyChain(message.getBytes))

    def digestSize[T : Hash]: Int = apply.digestSize

  }

}
