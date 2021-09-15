package co.topl.minting

import cats._
import cats.data.OptionT
import cats.implicits._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

/**
 * A `Mint` which produces "Unsigned" Blocks.  An UnsignedBlock has all of the components needed to form a BlockV2
 * except for a KES Certificate.  This allows for a delayed creation of a KES certificate until it is actually needed,
 * thus allowing for "cancellation" of this Mint attempt in the event that a better parent block arrives.
 */
class BlockMintProgram[F[_]: Monad] {

  def next(interpreter: BlockMintProgram.Algebra[F]): F[Option[BlockMintProgram.Out]] =
    interpreter.canonicalHead
      .map(_.headerV2)
      .flatMap(header =>
        OptionT(interpreter.elect(header)).semiflatMap {
          case BlockMintProgram.Election(slot, vrfCertificate, threshold) =>
            (interpreter.unconfirmedTransactions, interpreter.address).mapN { (transactions, address) =>
              BlockMintProgram.Out(
                timestamp =>
                  BlockHeaderV2.Unsigned(
                    parentHeaderId = header.id,
                    parentSlot = header.slot,
                    txRoot = transactions.merkleTree,
                    bloomFilter = transactions.bloomFilter,
                    timestamp = timestamp,
                    height = header.height + 1,
                    slot = slot,
                    vrfCertificate = vrfCertificate,
                    thresholdEvidence = threshold.evidence,
                    metadata = None,
                    address = address
                  ),
                transactions
              )
            }
        }.value
      )

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
