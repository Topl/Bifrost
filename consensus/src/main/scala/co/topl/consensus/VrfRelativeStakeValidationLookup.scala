package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
import co.topl.consensus.algebras.VrfRelativeStakeValidationLookupAlgebra
import co.topl.models.{Slot, TaktikosAddress, TypedIdentifier}

object VrfRelativeStakeValidationLookup {

  object Eval {

    def make[F[_]: Monad](
      state: BlockchainState[F],
      clock: ClockAlgebra[F]
    ): VrfRelativeStakeValidationLookupAlgebra[F] =
      (slotId: (Slot, TypedIdentifier), address: TaktikosAddress) =>
        clock
          .epochOf(slotId._1)
          .map(e => (e - 1).max(0))
          .flatMap(state.lookupRelativeStake(_)(address))
  }

}
