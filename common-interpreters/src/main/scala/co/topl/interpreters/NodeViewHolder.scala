package co.topl.interpreters

import cats.Applicative
import cats.implicits._
import co.topl.algebras.ConsensusStateReader
import co.topl.models._
import co.topl.models.utility.Ratio

object NodeViewHolder {

  object StaticData {

    def make[F[_]: Applicative](
      relativeStakes: Map[StakingAddress, Ratio],
      registrations:  Map[StakingAddress, Box.Values.Registrations.Pool]
    ): F[ConsensusStateReader[F]] =
      (new ConsensusStateReader[F] {

        def lookupRelativeStake(epoch: Epoch)(address: StakingAddress): F[Option[Ratio]] =
          relativeStakes.get(address).pure[F]

        def lookupRegistration(epoch: Epoch)(address: StakingAddress): F[Option[Box.Values.Registrations.Pool]] =
          registrations.get(address).pure[F]
      }).pure[F]

  }
}
