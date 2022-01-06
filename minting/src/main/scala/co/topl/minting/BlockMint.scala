package co.topl.minting

import cats._
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras._
import co.topl.minting.algebras.{BlockMintAlgebra, StakingAlgebra}
import co.topl.models._
import co.topl.typeclasses.implicits._

/**
 * A `Mint` which produces "Unsigned" Blocks.  An UnsignedBlock has all of the components needed to form a BlockV2
 * except for a KES Certificate.  This allows for a delayed creation of a KES certificate until it is actually needed,
 * thus allowing for "cancellation" of this Mint attempt in the event that a better parent block arrives.
 */
object BlockMint {

  object Eval {

    def make[F[_]: Monad](
      staker: StakingAlgebra[F],
      clock:  ClockAlgebra[F]
    ): BlockMintAlgebra[F] = { (parent: BlockHeaderV2, transactions: Seq[Transaction], slot: Slot) =>
      OptionT(staker.elect(parent, slot))
        .flatMapF(hit =>
          (staker.address, clock.currentTimestamp)
            .mapN((address, timestamp) =>
              (partialOperationalCertificate: BlockHeaderV2.Unsigned.PartialOperationalCertificate) =>
                BlockV2
                  .Unsigned(
                    BlockHeaderV2.Unsigned(
                      parentHeaderId = parent.id,
                      parentSlot = parent.slot,
                      txRoot = transactions.merkleTree,
                      bloomFilter = transactions.bloomFilter,
                      timestamp = timestamp,
                      height = parent.height + 1,
                      slot = hit.slot,
                      eligibilityCertificate = hit.cert,
                      partialOperationalCertificate = partialOperationalCertificate,
                      metadata = None,
                      address = address
                    ),
                    transactions
                  )
            )
            .flatMap(staker.certifyBlock(parent.parentSlotId, slot, _))
        )
        .value
    }
  }
}
