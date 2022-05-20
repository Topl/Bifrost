package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusStateReader}
import co.topl.consensus.algebras.RegistrationLookupAlgebra
import co.topl.models.{SlotId, StakingAddress}

object RegistrationLookup {

  object Eval {

    def make[F[_]: Monad](state: ConsensusStateReader[F], clock: ClockAlgebra[F]): RegistrationLookupAlgebra[F] =
      (slotId: SlotId, address: StakingAddress) =>
        clock
          .epochOf(slotId.slot)
          .flatMap(epoch => state.lookupRegistration(epoch)(address))
  }
}
