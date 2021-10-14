package co.topl.minting

import cats.Applicative
import cats.implicits._
import co.topl.crypto.{KeyIndex, KeyIndexes}
import co.topl.crypto.signing.ExtendedEd25519
import co.topl.minting.algebras.KeyEvolverAlgebra
import co.topl.models.utility.Bip32Index
import co.topl.models.{SecretKeys, Slot}

object KeyEvolver {

  object InMemory {

    def make[F[_]: Applicative](
      parent: SecretKeys.ExtendedEd25519
    ): KeyEvolverAlgebra[KeyIndexes.Bip32, F] =
      new KeyEvolverAlgebra[KeyIndexes.Bip32, F] {

        private val xEd25519 = new ExtendedEd25519()

        // todo: I am hard coding the hard index here but in general you would want to have this be calculated I think
        def evolveKey(index: KeyIndexes.Bip32): F[SecretKeys.ExtendedEd25519] = {
          Applicative[F].pure(xEd25519.deriveSecret(parent, Bip32Index.hardened(index.value.toInt)))
        }
      }
  }
}
