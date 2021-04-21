package co.topl.crypto

import co.topl.crypto.hash.{Blake2b256, Sha256}
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
    def hash(input: Array[Byte]): Digest
    def hashWithPrefix(prefix: Byte, input: Array[Byte]*): Digest
    def byteArrayToDigest(bytes: Array[Byte]): Try[Digest]
  }

  object Hash {
    def apply[T : Hash]: Hash[T] = implicitly[Hash[T]]

    def apply[T : Hash](message: Array[Byte]): Digest = Hash[T].hash(message)

    def apply[T : Hash](prefix: Byte, message: Array[Byte]*): Digest = Hash[T].hashWithPrefix(prefix, message: _*)

    def apply[T : Hash](message: String): Digest = Hash(message.getBytes)

    def byteArrayToDigest[T : Hash](bytes: Array[Byte]): Try[Digest] = Hash[T].byteArrayToDigest(bytes)

    def digestSize[T : Hash]: Int = apply.digestSize
  }
}

object Tester extends App {
  import co.topl.crypto.hash.{Hash, Digest}

  def needsHash[H : Hash](value: Int): Option[Digest] = {
    Some(Hash(value.toString))
  }

  import Blake2b256._

  needsHash(1)
  needsHash(2)
  needsHash(5)
  needsHash[Sha256](1000)
}
