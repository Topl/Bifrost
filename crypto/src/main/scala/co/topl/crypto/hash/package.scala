package co.topl.crypto

import supertagged.TaggedType

import scala.util.Try

/* Forked from https://github.com/input-output-hk/scrypto */

package object hash {
  object Digest extends TaggedType[Array[Byte]]

  type Digest = Digest.Type

  /** A hash function which hashes a message into a digest of type T.
   * @tparam T the digest type
   */
  trait Hash[T] {
    val digestSize: Int
    def hash(message: Array[Byte]): Digest
    def hashWithPrefix(prefix: Byte, message: Array[Byte]): Digest
    def byteArrayToDigest(bytes: Array[Byte]): Try[Digest]
  }

  object Hash {
    def apply[T : Hash]: Hash[T] = implicitly[Hash[T]]

    def apply[T : Hash](message: Array[Byte]): Digest = Hash[T].hash(message)

    def apply[T : Hash](prefix: Byte, message: Array[Byte]): Digest = Hash[T].hashWithPrefix(prefix, message)

    def apply[T : Hash](message: String): Digest = Hash(message.getBytes)

    def byteArrayToDigest[T : Hash](bytes: Array[Byte]): Try[Digest] = Hash[T].byteArrayToDigest(bytes)

    def digestSize[T : Hash]: Int = apply.digestSize
  }
}
