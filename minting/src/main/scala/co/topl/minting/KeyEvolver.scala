package co.topl.minting

import cats.Applicative
import co.topl.minting.algebras.KeyEvolverAlgebra
import co.topl.models.{SecretKeys, Slot}
import co.topl.typeclasses.Evolves
import co.topl.typeclasses.implicits._

object KeyEvolver {

  object InMemory {

    def make[F[_]: Applicative](
      parent: SecretKeys.ExtendedEd25519
    ): KeyEvolverAlgebra[F] =
      new KeyEvolverAlgebra[F] {

        def evolveKey(slot: Slot): F[SecretKeys.ExtendedEd25519] = {
          val index = slot * 1 // hacky way to convert slot to Long
          Applicative[F].pure(Evolves[SecretKeys.ExtendedEd25519].evolve(parent, index))
        }
      }
  }
}
