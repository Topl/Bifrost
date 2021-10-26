package co.topl.consensus

import cats.Monad
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{ClockAlgebra, ConsensusState}
import co.topl.consensus.algebras.RegistrationLookupAlgebra
import co.topl.models.{Slot, TaktikosAddress, TypedIdentifier}

object RegistrationLookup {

  object Eval {

    def make[F[_]: Monad](state: ConsensusState[F], clock: ClockAlgebra[F]): RegistrationLookupAlgebra[F] =
      (slotId: (Slot, TypedIdentifier), address: TaktikosAddress) =>
        clock
          .epochOf(slotId._1)
          .flatMap(epoch => state.lookupRegistration(epoch)(address))
  }
}
