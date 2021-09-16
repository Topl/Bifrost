package co.topl.minting

import cats._
import cats.data.OptionT
import cats.implicits._
import co.topl.algebras.ClockAlgebra.implicits._
import co.topl.algebras._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.implicits._

/**
 * A `Mint` which produces "Unsigned" Blocks.  An UnsignedBlock has all of the components needed to form a BlockV2
 * except for a KES Certificate.  This allows for a delayed creation of a KES certificate until it is actually needed,
 * thus allowing for "cancellation" of this Mint attempt in the event that a better parent block arrives.
 */
object BlockMint {

  object Eval {

    def make[F[_]: Monad](
      address:                TaktikosAddress,
      clock:                  ClockAlgebra[F],
      leaderElection:         LeaderElectionAlgebra[F],
      vrfRelativeStakeLookup: VrfRelativeStakeLookupAlgebra[F],
      etaLookup:              EtaLookupAlgebra[F]
    ): BlockMintAlgebra[F] = new BlockMintAlgebra[F] {

      private def findHit(parent: BlockHeaderV2, fromSlot: Slot): F[Vrf.Hit] =
        clock
          .epochOf(fromSlot)
          .flatMap(clock.epochBoundary)
          .flatMap(epochBoundary =>
            etaLookup
              .etaOf(parent, fromSlot)
              .flatMap(eta =>
                OptionT(vrfRelativeStakeLookup.lookupAt(parent, fromSlot)(address))
                  .flatMap(relativeStake =>
                    OptionT(leaderElection.nextHit(relativeStake, parent.slot, epochBoundary.end, eta))
                  )
                  .getOrElseF(findHit(parent, (epochBoundary.end: Long) + 1))
              )
          )

      def mint(parent: BlockHeaderV2, transactions: Seq[Transaction]): F[Timestamp => BlockV2.Unsigned] =
        findHit(parent, parent.slot)
          .map { hit: Vrf.Hit => (timestamp: Timestamp) =>
            BlockV2.Unsigned(
              BlockHeaderV2.Unsigned(
                parentHeaderId = parent.id,
                parentSlot = parent.slot,
                txRoot = transactions.merkleTree,
                bloomFilter = transactions.bloomFilter,
                timestamp = timestamp,
                height = parent.height + 1,
                slot = hit.slot,
                vrfCertificate = hit.cert,
                thresholdEvidence = hit.threshold.evidence,
                metadata = None,
                address = address
              ),
              transactions
            )
          }
    }
  }
}

object BlockMintProgram {
  case class Election(slot: Slot, vrfCertificate: Vrf.Certificate, threshold: Ratio)

  case class Out(unsignedHeaderF: Timestamp => BlockHeaderV2.Unsigned, transactions: Seq[Transaction]) {

    def signed(kesCertificate: KesCertificate, timestamp: Timestamp): BlockV2 = {
      val unsignedHeader = unsignedHeaderF(timestamp)
      val header = BlockHeaderV2(
        parentHeaderId = unsignedHeader.parentHeaderId,
        parentSlot = unsignedHeader.parentSlot,
        txRoot = transactions.merkleTree,
        bloomFilter = transactions.bloomFilter,
        timestamp = unsignedHeader.timestamp,
        height = unsignedHeader.height,
        slot = unsignedHeader.slot,
        vrfCertificate = unsignedHeader.vrfCertificate,
        kesCertificate = kesCertificate,
        thresholdEvidence = unsignedHeader.thresholdEvidence,
        metadata = unsignedHeader.metadata,
        address = unsignedHeader.address
      )
      val body = BlockBodyV2(
        transactions = transactions,
        headerId = header.id
      )
      BlockV2(header, body)
    }
  }

  trait Algebra[F[_]] {

    /**
     * The staking address
     */
    def address: F[TaktikosAddress]

    /**
     * All valid unconfirmed transactions at the currentHead
     */
    def unconfirmedTransactions: F[Seq[Transaction]]

    /**
     * Elect the next slot and certificate based on the given parent
     */
    def elect(parent: BlockHeaderV2): F[Option[BlockMintProgram.Election]]

    def canonicalHead: F[BlockV2]
  }
}
