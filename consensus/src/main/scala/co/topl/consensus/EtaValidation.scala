package co.topl.consensus

import cats.data.OptionT
import cats.{Monad, MonadError}
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras.{BlockchainState, ClockAlgebra}
import co.topl.consensus.algebras.EtaValidationAlgebra
import co.topl.models.{Eta, Slot, TypedIdentifier}

object EtaValidation {

  object Eval {

    def make[F[_]: MonadError[*[_], Throwable]](
      state: BlockchainState[F],
      clock: ClockAlgebra[F]
    ): EtaValidationAlgebra[F] = (slotId: (Slot, TypedIdentifier)) =>
      clock
        .epochOf(slotId._1)
        .flatMap(epoch =>
          OptionT(state.lookupEta(epoch))
            .getOrElseF(new IllegalStateException(s"Eta not found for epoch=$epoch").raiseError[F, Eta])
        )
  }
}
