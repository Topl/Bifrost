package co.topl.minting

import cats.Monad
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras._
import co.topl.models._

/**
 * A Staker attempts to construct a signed block at some given slot using some given parent.
 * @tparam F
 */
trait Staker[F[_]] {

  def mintBlock(
    head:         BlockV2,
    transactions: List[Transaction],
    slot:         Slot
  ): F[Option[BlockV2]]
}

object Staker {

  object Eval {

    def make[F[_]: Monad](
      address:                TaktikosAddress,
      clock:                  ClockAlgebra[F],
      leaderElection:         LeaderElectionHitAlgebra[F],
      mint:                   BlockMintAlgebra[F],
      signer:                 BlockSigningAlgebra[F],
      vrfRelativeStakeLookup: VrfRelativeStakeLookupAlgebra[F],
      etaLookup:              EtaLookupAlgebra[F]
    ): Staker[F] =
      (head: BlockV2, transactions: List[Transaction], slot: Slot) =>
        etaLookup
          .etaOf(head.headerV2, head.headerV2.slot)
          .flatMap(eta =>
            OptionT(vrfRelativeStakeLookup.lookupAt(head.headerV2, head.headerV2.slot, address))
              .flatMap(relativeStake =>
                OptionT(leaderElection.getHit(relativeStake, slot, slot - head.headerV2.slot, eta))
              )
              .semiflatMap(hit =>
                mint
                  .mint(head.headerV2, transactions, hit)
                  .flatMap(out =>
                    clock
                      .currentTimestamp()
                      .map(out)
                      .flatMap(block => clock.delayedUntilSlot(block.unsignedHeader.slot))
                      // TODO Check for cancellation
                      .flatMap(_ => signer.sign(out))
                  )
              )
              .value
          )
  }
}
