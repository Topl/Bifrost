package co.topl.crypto.keyfile

import cats._
import cats.data.{Chain, OptionT}

/**
 * Represents the operations of a "secure" data store.  "Secure" means that references to underlying value arrays
 * are internally weakly referenced and may be zeroed out at any moment.  When an entry is deleted, the implementation
 * should run the necessary procedures to remove entries from disk (if applicable) and zero out in-memory representations.
 */
trait SecureStore[F[_]] {

  /**
   * Initializes and persists a new KeyFile
   */
  def write(data: SecureData): F[Unit]

  /**
   * Read a single value by name, if the value exists
   */
  def read(name: String): F[Option[SecureData]]

  /**
   * List the names of all entries in this store
   */
  def list: F[Chain[String]]

  /**
   * Securely deletes the data associated with the given name
   */
  def delete(name: String): F[Unit]
}

/**
 * Wraps a [[SecureBytes]] with a name
 */
case class SecureData(name: String, bytes: SecureBytes)

/**
 * Represents a reference to an in-memory byte array that can be de-allocated and zero'd out when needed.  The underlying
 * byte array cannot be directly accessed from the outside.  Consumers must traverse the bytes and form their own
 * results.
 */
class SecureBytes private (private var underlying: Option[Array[Byte]], val length: Int) {

  /**
   * Consume each of the bytes one-by-one within a fold operation.  The foldOp operation produces some domain-specific
   * intermediate state.  The runOp operation applies a side-effect to the resulting State.  If the data has already
   * been consumed, then neither the foldOp nor runOp will be invoked, and None will be returned.
   */
  def foldLeft[F[_]: Monad, State](initialState: => State)(
    foldOp:                                      (State, Byte) => State
  )(runOp:                                       State => F[Unit]): F[Option[Unit]] =
    OptionT
      .fromOption[F](
        this.synchronized(
          underlying.map(_.foldLeft(initialState)(foldOp))
        )
      )
      .semiflatMap(runOp)
      .value

  def erase(): Unit =
    this.synchronized(
      underlying.foreach { arr =>
        // First, zero the array
        arr.indices.foreach(arr(_) = 0)
        // Now free the strong reference to the underlying vaue
        underlying = None
      }
    )

  def isErased(): Boolean =
    this.synchronized(underlying.isEmpty)

}

object SecureBytes {
  def apply(bytes: Array[Byte]): SecureBytes = new SecureBytes(Some(bytes), bytes.length)
}
