package co.topl.minting

import cats.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
import co.topl.minting.algebras.VrfRelativeStakeMintingLookupAlgebra
import co.topl.models.utility.Ratio
import co.topl.models.{Slot, TaktikosAddress}
import ClockAlgebra.implicits._
import cats.FlatMap

object VrfRelativeStakeMintingLookup {

  object Eval {

    def make[F[_]: FlatMap](
      state: BlockchainState[F],
      clock: ClockAlgebra[F]
    ): VrfRelativeStakeMintingLookupAlgebra[F] =
      (globalSlot: Slot, address: TaktikosAddress) =>
        clock.epochOf(globalSlot).flatMap(epoch => state.lookupRelativeStake(epoch)(address))
  }
}
