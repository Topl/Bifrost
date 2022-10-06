package co.topl.genusLibrary.util

import com.typesafe.scalalogging.Logger

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
}
