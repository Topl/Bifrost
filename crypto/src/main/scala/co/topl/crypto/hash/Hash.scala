package co.topl.crypto.hash

import scala.util.Try

trait Hash[T] {
  type Message = Array[Byte]

  val digestSize: Int

  def apply(input: Message): T
  def apply(prefix: Byte, inputs: Array[Byte]*): T
  def byteArrayToDigest(bytes: Array[Byte]): Try[T]
}

/** Library of hashing functions. */
object Hash {

  /** Hashes the message.
   * @param message the message to hash
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the hashed message
   */
  def apply[T](message: Array[Byte])(implicit hashFunc: Hash[T]): T = hashFunc(message)

  /** Hashes the message with the given prefix.
   * @param prefix the prefix of the message
   * @param message the message to hash
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the hashed message
   */
  def apply[T](prefix: Byte, message: Array[Byte]*)(implicit hashFunc: Hash[T]): T = hashFunc(prefix, message: _*)

  /** Hashes the message.
   * @param message the message to hash
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the hashed message
   */
  def apply[T](message: String)(implicit hashFunc: Hash[T]): T = hashFunc(message.getBytes)

  /** Converts the byte array to the hashing function's digest type.
   * @param bytes the bytes to convert
   * @param hashFunc the implicit hashing function
   * @tparam T the digest type
   * @return the digest value if it the input is convertible
   */
  def byteArrayToDigest[T](bytes: Array[Byte])(implicit hashFunc: Hash[T]): Try[T] =
    hashFunc.byteArrayToDigest(bytes)

  /** Gets hashing function's digest size.
   * @param hashFunc the implicit hashing function to use
   * @return the digest size of the hashing function
   */
  def digestSize(implicit hashFunc: Hash[_]): Int = hashFunc.digestSize
}
