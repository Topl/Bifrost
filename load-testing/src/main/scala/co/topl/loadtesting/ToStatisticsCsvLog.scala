package co.topl.loadtesting

import simulacrum.typeclass

@typeclass
trait ToStatisticsCsvLog[T] {
  def toLog(t: T): String
}
