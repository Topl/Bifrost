package co.topl.interpreters

import cats.Applicative
import cats.implicits._
import co.topl.algebras.ConsensusStateReader
import co.topl.models._
import co.topl.models.utility.Ratio

object NodeViewHolder {

  object StaticData {

    def make[F[_]: Applicative](
      relativeStakes: Map[TaktikosAddress, Ratio],
      registrations:  Map[TaktikosAddress, Box.Values.TaktikosRegistration]
    ): F[ConsensusStateReader[F]] =
      (new ConsensusStateReader[F] {

        def lookupRelativeStake(epoch: Epoch)(address: TaktikosAddress): F[Option[Ratio]] =
          relativeStakes.get(address).pure[F]

        def lookupRegistration(epoch: Epoch)(address: TaktikosAddress): F[Option[Box.Values.TaktikosRegistration]] =
          registrations.get(address).pure[F]
      }).pure[F]

  }
}
