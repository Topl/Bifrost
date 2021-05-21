package co.topl.crypto

import cats.data.NonEmptyChain
import co.topl.crypto.hash.digest._

import scala.language.implicitConversions

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {

  type Message = Array[Byte]

  /**
   * Represents a hashing function with a scheme and digest type.
   *
   * @tparam H the hashing scheme
   * @tparam D the digest type with an implicit Digest implementation
   */
  abstract class Hash[H, D: Digest] {

    /**
     * Hashes a set of messages with an optional prefix.
     *
     * @param prefix the optional prefix byte of the hashed message
     * @param messages the set of messages to iteratively hash
     * @return the hash digest
     */
    def hash(prefix: Option[Byte], messages: Message*): D

    /**
     * Hashes a set of messages with a given prefix.
     *
     * @param prefix the prefix byte of the hashed message
     * @param messages the set of messages to iteratively hash
     * @return the hash digest
     */
    def hash(prefix: Byte, messages: Message*): D = hash(Some(prefix), messages: _*)

    /**
     * Hashes a message.
     *
     * @param message the message to hash
     * @return the hash digest
     */
    def hash(message: Message): D = hash(None, message)
  }

  object Hash {
    def apply[H, D: Digest](implicit hash: Hash[H, D]): Hash[H, D] = hash
  }

  type Blake2b
  type Sha

  trait Instances {
    implicit val sha256: Hash[Sha, Digest32] = Sha256
    implicit val sha512: Hash[Sha, Digest64] = Sha512
    implicit val blake2b256: Hash[Blake2b, Digest32] = Blake2b256
    implicit val blake2b512: Hash[Blake2b, Digest64] = Blake2b512
  }

  object implicits extends digest.Instances with digest.Digest.ToDigestOps with Instances
}
