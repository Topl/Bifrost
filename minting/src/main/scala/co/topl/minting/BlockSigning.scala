package co.topl.minting

import cats.Monad
import cats.implicits._
import co.topl.algebras.{BlockSigningAlgebra, ClockAlgebra, KeyEvolverAlgebra}
import co.topl.crypto.typeclasses.implicits._
import co.topl.models.{BlockHeaderV2, Timestamp}

object BlockSigning {

  object Eval {

    def make[F[_]: Monad](clockAlgebra: ClockAlgebra[F], evolver: KeyEvolverAlgebra[F]): BlockSigningAlgebra[F] =
      (unsignedBlockF: Timestamp => BlockHeaderV2.Unsigned) =>
        clockAlgebra
          .currentTimestamp()
          .map(unsignedBlockF(_))
          .flatMap(unsignedBlock => evolver.evolvedKey(unsignedBlock.slot).map(_.certify(unsignedBlock)))
  }
}
