package co.topl.loadtesting.statistics

import simulacrum.typeclass

/**
 * A type-class which can convert a value into a CSV friendly log.
 *
 * @tparam T the type of value to converts
 */
@typeclass
trait ToStatisticsCsvLog[T] {

  /**
   * Converts the value to a CSV friendly log message.
   * @param t the value to convert into CSV
   * @return the CSV log message
   */
  def toCsvLog(t: T): String
}
