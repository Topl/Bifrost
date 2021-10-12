package co.topl.minting

import cats.Applicative
import cats.implicits._
import co.topl.crypto.kes.KeyEvolvingSignatureScheme
import co.topl.crypto.typeclasses.implicits._
import co.topl.minting.algebras.KeyEvolverAlgebra
import co.topl.models.{SecretKeys, Slot}

object KeyEvolver {

  object InMemory {

    def make[F[_]: Applicative](
      initialKey:                          SecretKeys.SymmetricMMM
    )(implicit keyEvolvingSignatureScheme: KeyEvolvingSignatureScheme): KeyEvolverAlgebra[F] =
      new KeyEvolverAlgebra[F] {

        private var key = initialKey

        def evolvedKey(slot: Slot): F[SecretKeys.SymmetricMMM] = {
          key = key.evolveSteps(slot - key.data.offset)
          key.pure[F]
        }
      }
  }
}
