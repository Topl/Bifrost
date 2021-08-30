package co.topl.minting

import cats._
import cats.syntax.flatMap._
import cats.syntax.functor._
import co.topl.models._
import co.topl.models.utility.Ratio
import co.topl.typeclasses.ContainsEvidence.Instances._
import co.topl.typeclasses.ContainsEvidence.ops._
import co.topl.typeclasses.ContainsTransactions.Instances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

/**
 * A `Mint` which produces Blocks.
 */
class BlockMint[F[_]: Monad](interpreter: BlockMint.Algebra[F]) extends Mint[F, BlockV2] {

  override def nextValueAfter(parentBlock: BlockV2): F[BlockV2] = {
    val BlockMint.Election(slot, vrfCertificate, threshold) = interpreter.elect(parentBlock.headerV2)
    interpreter.unconfirmedTransactions(parentBlock).flatMap { transactions =>
      interpreter.nextKesCertificate(slot).map { kesCertificate =>
        val timestamp = interpreter.currentTime()
        val header = BlockHeaderV2(
          parentHeaderId = parentBlock.headerV2.id,
          txRoot = transactions.merkleTree,
          bloomFilter = transactions.bloomFilter,
          timestamp = timestamp,
          height = parentBlock.headerV2.height + 1,
          slot = slot,
          vrfCertificate = vrfCertificate,
          kesCertificate = kesCertificate,
          thresholdEvidence = threshold.evidence,
          metadata = None,
          address = interpreter.address
        )
        val body = BlockBodyV2(
          transactions = transactions,
          headerId = header.id
        )
        BlockV2(header, body)
      }
    }
  }
}

object BlockMint {
  case class Election(slot: Slot, vrfCertificate: VrfCertificate, threshold: Ratio)

  trait Algebra[F[_]] {

    /**
     * The staking address
     */
    def address: TaktikosAddress

    /**
     * The current global-clock timestamp
     */
    def currentTime(): Timestamp

    /**
     * All valid unconfirmed transactions at the particular block
     */
    def unconfirmedTransactions(block: BlockV2): F[Seq[Transaction]]

    /**
     * Elect the next slot and certificate based on the given parent
     */
    def elect(parent: BlockHeaderV2): BlockMint.Election

    /**
     * The KES certificate for the particular target slot
     */
    def nextKesCertificate(slot: Slot): F[KesCertificate]
  }
}
