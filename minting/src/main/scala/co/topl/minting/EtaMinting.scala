package co.topl.minting

import cats.MonadError
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.minting.algebras.EtaMintingAlgebra
import co.topl.models.{Eta, Slot}

object EtaMinting {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]](
      state: ConsensusState[F],
      clock: ClockAlgebra[F]
    ): EtaMintingAlgebra[F] =
      (globalSlot: Slot) =>
        clock
          .epochOf(globalSlot)
          .map(_ - 1)
          .flatMap(epoch =>
            OptionT(state.lookupEta(epoch))
              .getOrElseF(new IllegalStateException(s"Eta not found for epoch=$epoch").raiseError[F, Eta])
          )
  }
}
