package co.topl.minting

import cats.FlatMap
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusStateReader}
import co.topl.minting.algebras.VrfRelativeStakeMintingLookupAlgebra
import co.topl.models.{Slot, StakingAddress}

object VrfRelativeStakeMintingLookup {

  object Eval {

    def make[F[_]: FlatMap](
      state: ConsensusStateReader[F],
      clock: ClockAlgebra[F]
    ): VrfRelativeStakeMintingLookupAlgebra[F] =
      (globalSlot: Slot, address: StakingAddress) =>
        clock.epochOf(globalSlot).flatMap(epoch => state.lookupRelativeStake(epoch)(address))
  }
}
