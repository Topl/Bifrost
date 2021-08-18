package co.topl.minting

import co.topl.models._
import co.topl.typeclasses.ContainsHeight.Instances._
import co.topl.typeclasses.ContainsHeight.ops._
import co.topl.typeclasses.Identifiable.Instances._
import co.topl.typeclasses.Identifiable.ops._

import scala.concurrent.{ExecutionContext, Future}

/**
 * A `Mint` which produces Blocks.
 * @param getCurrentTime A function which fetches the current timestamp
 * @param nextTransactions A function which fetches a collection of transactions that are valid based on the
 *                         provided (head/parent) Block. This should include "Reward" transactions.
 * @param elect A function which asynchronously determines the slot and certificates for the next Block
 */
class BlockMint(
  getCurrentTime:   () => Timestamp,
  nextTransactions: Block => Future[Seq[Transaction]],
  elect:            Block => Future[BlockMint.Election]
)(implicit ec:      ExecutionContext)
    extends Mint[Block] {

  override def nextValue(parentBlock: Block): Future[Block] =
    for {
      BlockMint.Election(slot, vrfCertificate, kesCertificate) <- elect(parentBlock)
      transactions                                             <- nextTransactions(parentBlock)
      timestamp = getCurrentTime()
    } yield BlockV2(
      parentId = parentBlock.id,
      timestamp = timestamp,
      height = parentBlock.height,
      transactions = transactions,
      slot = slot,
      vrfCertificate = vrfCertificate,
      kesCertificate = kesCertificate
    )
}

object BlockMint {
  case class Election(slot: Slot, vrfCertificate: VrfCertificate, kesCertificate: KesCertificate)
}
