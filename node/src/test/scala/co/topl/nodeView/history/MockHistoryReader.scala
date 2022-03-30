package co.topl.nodeView.history

import cats.implicits._
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.TX
import co.topl.network.BifrostSyncInfo
import co.topl.nodeView.history.GenericHistory.ModifierIds
import co.topl.utils.TimeProvider.Time

import scala.util.Try

/**
 * A mock history reader which uses an internal list of blocks as its history. The last block in the list is the
 * current best block.
 *
 * @param blocks the list of blocks which make up the history
 */
class MockHistoryReader(blocks: List[Block]) extends HistoryReader[Block, BifrostSyncInfo] {
  override def height: Long = blocks.length

  override def bestBlock: Block = blocks.lastOption.get

  override def difficulty: Long = bestBlock.difficulty

  override def bestBlockId: ModifierId = bestBlock.id

  override def score: Long = 0

  override def isEmpty: Boolean = blocks.isEmpty

  override def filter(f: Block => Boolean): Seq[Block] = blocks.filter(f)

  override def modifierByHeight(height: Long): Option[Block] = blocks.find(_.height == height)

  override def parentBlock(m: Block): Option[Block] =
    blocks.find(_ == m).map(_.parentId).flatMap(parentId => blocks.find(_.id == parentId))

  override def getTimestampsFrom(startBlock: Block, count: Long): Vector[Time] =
    blocks
      .reverse
      .takeWhile(_ != startBlock)
      .reverse
      .take(count.toInt)
      .map(_.timestamp)
      .toVector

  override def transactionById(id: ModifierId): Option[(TX, ModifierId, Long)] =
    blocks
      .flatMap(block =>
          block.transactions
            .map(tx => (tx, block.id, block.height))
      )
      .find(_._1.id == id)

  override def applicableTry(modifier: Block): Try[Unit] = throw new NotImplementedError()

  override def openSurfaceIds(): Seq[ModifierId] = throw new NotImplementedError()

  override def continuationIds(info: BifrostSyncInfo, size: Int): ModifierIds = throw new NotImplementedError()

  override def syncInfo: BifrostSyncInfo = throw new NotImplementedError()

  override def compare(other: BifrostSyncInfo): GenericHistory.HistoryComparisonResult = throw new NotImplementedError()

  override def extendsKnownTine(modifier: Block): Boolean = throw new NotImplementedError()

  override def idAtHeightOf(height: Long): Option[ModifierId] = blocks.find(_.height == height).map(_.id)

  override def getIdsFrom(startBlock: Block, until: Block => Boolean, limit: Int): Option[Seq[ModifierId]] =
    blocks
      .reverse
      .takeWhile(_ != startBlock)
      .reverse
      .takeWhile(block => !until(block))
      .take(limit)
      .map(_.id)
      .some

  override def modifierById(modifierId: ModifierId): Option[Block] = blocks.find(_.id == modifierId)

  override type NVCT = this.type
}

object MockHistoryReader {
  def apply(blocks: List[Block]): HistoryReader[Block, BifrostSyncInfo] = new MockHistoryReader(blocks)
}
