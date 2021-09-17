package co.topl.minting

import cats.Applicative
import cats.implicits._
import co.topl.algebras.KeyEvolverAlgebra
import co.topl.crypto.typeclasses.ContainsVerificationKey
import co.topl.crypto.typeclasses.implicits._
import co.topl.models.{PrivateKeys, PublicKeys, Slot}

object KeyEvolver {

  object InMemory {

    def make[F[_]: Applicative](initialKey: PrivateKeys.Kes): KeyEvolverAlgebra[F] =
      new KeyEvolverAlgebra[F] {

        private var key = initialKey

        def evolvedKey(slot: Slot): F[PrivateKeys.Kes] = {
          key = key.evolveSteps(
            slot - ContainsVerificationKey[PrivateKeys.Kes, PublicKeys.Kes].verificationKeyOf(key).offset
          )
          key.pure[F]
        }
      }
  }
}
