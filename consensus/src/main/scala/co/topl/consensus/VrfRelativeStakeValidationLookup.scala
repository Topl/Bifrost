package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.VrfRelativeStakeValidationLookupAlgebra
import co.topl.models.{Slot, SlotId, TaktikosAddress, TypedIdentifier}

object VrfRelativeStakeValidationLookup {

  object Eval {

    def make[F[_]: Monad](
      state: ConsensusState[F],
      clock: ClockAlgebra[F]
    ): VrfRelativeStakeValidationLookupAlgebra[F] =
      (slotId: SlotId, address: TaktikosAddress) =>
        clock
          .epochOf(slotId.slot)
          .map(e => (e - 1).max(0))
          .flatMap(state.lookupRelativeStake(_)(address))
  }

}
