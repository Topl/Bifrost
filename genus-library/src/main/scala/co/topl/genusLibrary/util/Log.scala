package co.topl.genusLibrary.util

import com.typesafe.scalalogging.Logger

import scala.util.{Failure, Try}

/**
 * Log, at info level, the result of an expression like this:
 * {{{
 *   Log.info("f returned {}") {
 *     f(oneArg, anotherArg)
 *   }
 * }}}
 */
object Log {
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
  def debug [T](message: String)( body: => T)(implicit logger: Logger): T = {
    val result:T = body
    logger.debug (message, result)
    result
  }

  /**
   * Log the given Try if it is a Failure
   *
   * @param message the message to log
   * @param t The Try that will be logged if it is a Failure
   * @tparam T The type of the Try
   * @return the Try
   */
  def ifFailure[T](message: String)(t: Try[T])(implicit logger: Logger): Try[T] = {
    t.recoverWith(f => {
      logger.error(message, f)
      Failure(f)
    })
  }
}
