package co.topl.crypto

import cats.data.{NonEmptyChain, Validated}
import io.estatico.newtype.macros.newtype

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

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

  /** A hash function which hashes a message into a digest of type T.
   * @tparam T the digest type
   */
  trait Hash[T, D] {
    def hash(prefix: Option[Byte], messages: NonEmptyChain[Array[Byte]]): D
  }

  object Hash {

    /** Instantiates the hash function from the implicit scope.
     * @tparam T the hash type
     * @return the hash function
     */
    def apply[T, D](implicit hash: Hash[T, D]) : Hash[T, D] = implicitly[Hash[T, D]]

    /** Hashes the given message.
     * @param message the message to hash
     * @tparam T the hash type
     * @return the hash digest
     */
    def apply[T, D](message: Array[Byte])(implicit hash: Hash[T, D]): D =
      apply.hash(None, NonEmptyChain(message))

    /** Hashes the given set of messages with a prefix
     * @param prefix the prefix to apply to the hash
     * @param messages the messages to hash
     * @tparam T the hash type
     * @return the hash digest
     */
    def apply[T, D](prefix: Byte, messages: NonEmptyChain[Array[Byte]])(implicit hash: Hash[T, D]): D =
      apply.hash(Some(prefix), messages)

    /** Hashes the given message with a prefix.
     * @param prefix the prefix to apply to the hash
     * @param message the message to hash
     * @tparam T the hash type
     * @return a hashed digest
     */
    def apply[T, D](prefix: Byte, message: Array[Byte])(implicit hash: Hash[T, D]): D =
      apply.hash(Some(prefix), NonEmptyChain(message))

    /** Hashes the given message.
     * @param message the message to hash
     * @tparam T the hash type
     * @return the hash digest
     */
    def apply[T, D](message: String)(implicit hash: Hash[T, D]): D =
      apply.hash(None, NonEmptyChain(message.getBytes))

  }

}
