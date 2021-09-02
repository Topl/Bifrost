package co.topl.minting

import cats._
import cats.syntax.functor._
import co.topl.algebras.Clock
import co.topl.models._
import co.topl.models.utility.StringDataTypes.Latin1Data
import co.topl.models.utility.{Lengths, Ratio, Sized}
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
class BlockMint[F[_]: Monad] extends Mint[F, BlockMint.UnsignedBlock, BlockMint.Algebra[F]] {

  override def next(interpreter: BlockMint.Algebra[F]): F[BlockMint.UnsignedBlock] = {
    val BlockMint.Election(slot, vrfCertificate, threshold) = interpreter.elect(interpreter.currentHead.headerV2)
    interpreter.unconfirmedTransactions.map { transactions =>
      BlockMint.UnsignedBlock(
        parentHeaderId = interpreter.currentHead.headerV2.id,
        txRoot = transactions.merkleTree,
        bloomFilter = transactions.bloomFilter,
        height = interpreter.currentHead.headerV2.height + 1,
        slot = slot,
        vrfCertificate = vrfCertificate,
        thresholdEvidence = threshold.evidence,
        metadata = None,
        address = interpreter.address,
        transactions = transactions
      )
    }
  }

}

object BlockMint {
  case class Election(slot: Slot, vrfCertificate: VrfCertificate, threshold: Ratio)

  case class UnsignedBlock(
    parentHeaderId:    TypedIdentifier,
    txRoot:            TxRoot,
    bloomFilter:       BloomFilter,
    height:            Long,
    slot:              Slot,
    vrfCertificate:    VrfCertificate,
    thresholdEvidence: Evidence,
    metadata:          Option[Sized.Max[Latin1Data, Lengths.`32`.type]],
    address:           TaktikosAddress,
    transactions:      Seq[Transaction]
  ) {

    def signed[F[_]](kesCertificate: KesCertificate)(implicit clock: Clock[F]): BlockV2 = {
      val header = BlockHeaderV2(
        parentHeaderId = parentHeaderId,
        txRoot = transactions.merkleTree,
        bloomFilter = transactions.bloomFilter,
        timestamp = clock.currentTimestamp(),
        height = height,
        slot = slot,
        vrfCertificate = vrfCertificate,
        kesCertificate = kesCertificate,
        thresholdEvidence = thresholdEvidence,
        metadata = metadata,
        address = address
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
    def address: TaktikosAddress

    /**
     * All valid unconfirmed transactions at the currentHead
     */
    def unconfirmedTransactions: F[Seq[Transaction]]

    /**
     * Elect the next slot and certificate based on the given parent
     */
    def elect(parent: BlockHeaderV2): BlockMint.Election

    def currentHead: BlockV2

    def clock: Clock[F]
  }
}
