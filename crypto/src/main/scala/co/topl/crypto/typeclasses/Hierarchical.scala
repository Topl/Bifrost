package co.topl.crypto.typeclasses

import simulacrum.typeclass

@typeclass trait Hierarchical[T] {
  def derive(t: T, index: Long): T
}
