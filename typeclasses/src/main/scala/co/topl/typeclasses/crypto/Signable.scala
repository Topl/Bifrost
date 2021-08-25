package co.topl.typeclasses.crypto

import co.topl.models.Bytes
import simulacrum.typeclass

@typeclass trait Signable[T] {
  def signableBytes(t: T): Bytes
}
