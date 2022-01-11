package co.topl.crypto.keyfile

import cats.data.Chain
import co.topl.codecs.bytes.ByteCodec

/**
 * Represents the operations of a "secure" data store.  "Secure" means that reading a value will immediately erase its
 * persistent representation.
 */
trait SecureStore[F[_]] {

  /**
   * Write the given data to disk using the provided codec
   */
  def write[A: ByteCodec](name: String, data: A): F[Unit]

  /**
   * Read a single value by name into some type `A`, and erase the persisted
   * representation.
   *
   * TODO: (preErase: A => F[Unit])
   */
  def consume[A: ByteCodec](name: String): F[Option[A]]

  /**
   * List the names of all entries in this store
   */
  def list: F[Chain[String]]

  /**
   * Securely deletes the data associated with the given name
   */
  def erase(name: String): F[Unit]
}
