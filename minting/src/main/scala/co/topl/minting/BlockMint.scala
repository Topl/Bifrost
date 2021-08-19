package co.topl.minting

import co.topl.models._
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
  nextTransactions: BlockV2 => Future[Seq[Transaction]],
  elect:            BlockHeaderV2 => Future[BlockMint.Election]
)(implicit ec:      ExecutionContext)
    extends Mint[BlockV2] {

  override def nextValue(parentBlock: BlockV2): Future[BlockV2] =
    for {
      BlockMint.Election(slot, vrfCertificate, kesCertificate) <- elect(parentBlock.headerV2)
      transactions                                             <- nextTransactions(parentBlock)
      timestamp = getCurrentTime()
      body = BlockBodyV2(
        transactions = transactions,
        parentHeaderId = parentBlock.headerV2.id
      )
      header = BlockHeaderV2(
        parentHeaderId = parentBlock.headerV2.id,
        blockBodyId = body.id,
        timestamp = timestamp,
        height = parentBlock.headerV2.height,
        slot = slot,
        vrfCertificate = vrfCertificate,
        kesCertificate = kesCertificate
      )
    } yield BlockV2(header, body)
}

object BlockMint {
  // TODO: `kesCertificate` isn't part of the election process
  case class Election(slot: Slot, vrfCertificate: VrfCertificate, kesCertificate: KesCertificate)
}
