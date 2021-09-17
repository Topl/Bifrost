package co.topl.minting

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras._
import co.topl.models._

object Staking {

  object Eval {

    def make[F[_]: Monad](
      a:                      TaktikosAddress,
      leaderElection:         LeaderElectionHitAlgebra[F],
      signer:                 BlockSigningAlgebra[F],
      vrfRelativeStakeLookup: VrfRelativeStakeLookupAlgebra[F],
      etaLookup:              EtaLookupAlgebra[F]
    ): StakingAlgebra[F] = new StakingAlgebra[F] {
      def address: F[TaktikosAddress] = a.pure[F]

      def elect(parent: BlockHeaderV2, slot: Slot): F[Option[Vrf.Hit]] =
        etaLookup
          .etaOf(parent, parent.slot)
          .flatMap(eta =>
            OptionT(vrfRelativeStakeLookup.lookupAt(parent, parent.slot, a))
              .flatMap(relativeStake => OptionT(leaderElection.getHit(relativeStake, slot, slot - parent.slot, eta)))
              .value
          )

      def certifyBlock(unsigned: BlockV2.Unsigned): F[BlockV2] =
        signer.sign(unsigned)
    }
  }

}
