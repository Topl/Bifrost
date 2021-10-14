package co.topl.minting.algebras

import co.topl.crypto.KeyIndex

/**
 * A KeyEvolverAlgebra is responsible for encapsulating a key locally and emitting an evolved version of
 * it for some particular slot on demand
 */
trait KeyEvolverAlgebra[T, F[_]] {
  def evolveKey(index: T): F[_]
}
