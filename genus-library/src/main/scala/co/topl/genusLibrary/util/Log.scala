package co.topl.genusLibrary.util

import com.typesafe.scalalogging.Logger

import scala.util.{Failure, Try}

/**
 * Functions to make logging compatible with functional programming style
 */
object Log {
  /**
   * Log, at info level, the result of an expression like this:
   * {{{
   *   Log.info("f returned {}") {
   *     f(oneArg, anotherArg)
   *   }
   * }}}
   */
  def info[T](message: String)( body: => T)(implicit logger: Logger): T = {
    val result:T = body
    logger.info(message, result)
    result
  }

/**
 * Log, at debug level, the result of an expression like this:
 * {{{
 *   Log.debug ("f returned {}") {
 *     f(oneArg, anotherArg)
 *   }
 * }}}
 */
  def debug[T](message: String)( body: => T)(implicit logger: Logger): T = {
    val result:T = body
    logger.debug (message, result)
    result
  }

  implicit class TryLogging[T](t: Try[T]) {
    /**
     * Log the given Try if it is a Failure
     *
     * @param message the message to log
     * @return the Try
     */
    def logIfFailure(message: String)(implicit logger: Logger): Try[T] = {
      t.recoverWith(f => {
        logger.error(message, f)
        Failure(f)
      })
    }
  }
}
