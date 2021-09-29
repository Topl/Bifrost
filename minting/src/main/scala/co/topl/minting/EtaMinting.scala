package co.topl.minting

import cats.MonadError
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
import co.topl.minting.algebras.EtaMintingAlgebra
import co.topl.models.{Eta, Slot}

object EtaMinting {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]](
      state: BlockchainState[F],
      clock: ClockAlgebra[F]
    ): EtaMintingAlgebra[F] =
      (globalSlot: Slot) =>
        OptionT(clock.epochOf(globalSlot).flatMap(state.lookupEta))
          .getOrElseF(new IllegalStateException(s"Eta not found for slot=$globalSlot").raiseError[F, Eta])
  }
}
