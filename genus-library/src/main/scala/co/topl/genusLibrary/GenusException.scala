package co.topl.genusLibrary

/**
 * Base exception class for the Genus library.
 *
 * @constructor Create a GenusException
 * @param message the detail message (which is saved for later retrieval by the getMessage() method).
 * @param cause the cause (which is saved for later retrieval by the getCause() method). (A null value is permitted,
 *              and indicates that the cause is nonexistent or unknown.)
 */
class GenusException(message: String, cause: Throwable) extends RuntimeException(message, cause) {}

object GenusException {
  /**
   * Create a GenusException
   *
   * @param message the detail message (which is saved for later retrieval by the getMessage() method).
   * @param cause   the cause (which is saved for later retrieval by the getCause() method). (A null value is permitted,
   *                and indicates that the cause is nonexistent or unknown.)
   */
  def apply(message: String, cause: Throwable): GenusException = new GenusException(message, cause)

  /**
   * Create a GenusException with no underlying exception/cause.
   *
   * @param message  the detail message (which is saved for later retrieval by the getMessage() method).
   */
  def apply(message: String): GenusException = new GenusException(message, null)
}
