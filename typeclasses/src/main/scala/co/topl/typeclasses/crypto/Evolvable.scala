package co.topl.typeclasses.crypto

import simulacrum.typeclass

@typeclass trait Evolvable[T] {
  def evolve(t: T): T
}
