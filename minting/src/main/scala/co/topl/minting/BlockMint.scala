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
 * @param getCurrentTime A function which fetches the current timestamp
 * @param nextTransactions A function which fetches a collection of transactions that are valid based on the
 *                         provided (head/parent) Block. This should include "Reward" transactions.
 * @param elect A function which asynchronously determines the slot and certificates for the next Block
 */
class BlockMint[F[_]](
  address:            TaktikosAddress,
  getCurrentTime:     () => Timestamp,
  nextTransactions:   BlockV2 => F[Seq[Transaction]],
  elect:              BlockHeaderV2 => BlockMint.Election,
  nextKesCertificate: Slot => F[KesCertificate]
)(implicit fMonad:    Monad[F])
    extends Mint[F, BlockV2] {

  override def nextValueAfter(parentBlock: BlockV2): F[BlockV2] = {
    val BlockMint.Election(slot, vrfCertificate, threshold) = elect(parentBlock.headerV2)
    nextTransactions(parentBlock).flatMap { transactions =>
      nextKesCertificate(slot).map { kesCertificate =>
        val timestamp = getCurrentTime()
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
          address = address
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
}
