package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.RegistrationLookupAlgebra
import co.topl.models.{Slot, SlotId, TaktikosAddress, TypedIdentifier}

object RegistrationLookup {

  object Eval {

    def make[F[_]: Monad](state: ConsensusState[F], clock: ClockAlgebra[F]): RegistrationLookupAlgebra[F] =
      (slotId: SlotId, address: TaktikosAddress) =>
        clock
          .epochOf(slotId.slot)
          .flatMap(epoch => state.lookupRegistration(epoch)(address))
  }
}
