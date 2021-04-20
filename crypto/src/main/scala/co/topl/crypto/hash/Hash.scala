package co.topl.crypto.hash

import supertagged.TaggedType

import scala.util.Try

object Hash {

  trait BaseDigest extends TaggedType[Array[Byte]]

  type Digest = BaseDigest#Type

  object Digest32 extends BaseDigest

  type Digest32 = Digest32.Type

  object Digest64 extends BaseDigest

  type Digest64 = Digest64.Type

  object NonStandardDigest extends BaseDigest

  type NonStandardDigest = NonStandardDigest.Type

  /** Hashes the message.
   * @param message the message to hash
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the hashed message
   */
  def apply[T](message: Array[Byte])(implicit hashFunc: HashFunction[T]): T = hashFunc(message)

  /** Hashes the message with the given prefix.
   * @param prefix the prefix of the message
   * @param message the message to hash
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the hashed message
   */
  def apply[T](prefix: Byte, message: Array[Byte]*)(implicit hashFunc: HashFunction[T]): T =
    hashFunc(prefix, message: _*)

  /** Hashes the message.
   * @param message the message to hash
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the hashed message
   */
  def apply[T](message: String)(implicit hashFunc: HashFunction[T]): T = hashFunc(message.getBytes)

  /** Converts the byte array to the hashing function's digest type.
   * @param bytes the bytes to convert
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the digest value if it the input is convertible
   */
  def byteArrayToDigest[T](bytes: Array[Byte])(implicit hashFunc: HashFunction[T]): Try[T] =
    hashFunc.byteArrayToDigest(bytes)

  /** Gets hashing function's digest size.
   * @param hashFunc the implicit hashing function to use
   * @return the digest size of the hashing function
   */
  def digestSize(implicit hashFunc: HashFunction[_]): Int = hashFunc.digestSize

}
