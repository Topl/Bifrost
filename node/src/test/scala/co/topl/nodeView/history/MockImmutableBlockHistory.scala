package co.topl.nodeView.history

import co.topl.consensus.BlockValidator
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction.TX
import co.topl.network.BifrostSyncInfo
import co.topl.nodeView.history.GenericHistory.ModifierIds
import co.topl.utils.TimeProvider.Time
import cats.implicits._

import scala.util.Try

/**
 * An immutable mock block history. Will throw a [[NotImplementedError]] when mutate functions are called.
 * @param blocks the immutable set of blocks representing history
 */
class MockImmutableBlockHistory(blocks: List[Block])
    extends GenericHistory[Block, BifrostSyncInfo, History] {
  override type NVCT = this.type

  override def isEmpty: Boolean = blocks.isEmpty

  override def modifierById(modifierId: ModifierId): Option[Block] = blocks.find(_.id == modifierId)

  override def modifierByHeight(height: Long): Option[Block] = blocks.find(_.height == height)

  override def append(
    modifier:   Block,
    validators: Seq[BlockValidator[_]]
  ): Try[(History, GenericHistory.ProgressInfo[Block])] = throw new NotImplementedError()

  override def drop(modifierId: ModifierId): History = throw new NotImplementedError()

  override def openSurfaceIds(): Seq[ModifierId] = Seq.empty

  override def continuationIds(from: ModifierIds, size: Int): Option[ModifierIds] = None

  override def syncInfo: BifrostSyncInfo = throw new NotImplementedError()

  override def reportModifierIsValid(modifier: Block): History = throw new NotImplementedError()

  override def reportModifierIsInvalid(
    modifier:     Block,
    progressInfo: GenericHistory.ProgressInfo[Block]
  ): (History, GenericHistory.ProgressInfo[Block]) = throw new NotImplementedError()

  override def height: Long = blocks.length

  override def bestBlock: Block = blocks.last

  override def difficulty: Long = bestBlock.difficulty

  override def bestBlockId: ModifierId = bestBlock.id

  override def score: Long = throw new NotImplementedError()

  override def filter(f: Block => Boolean): Seq[Block] = blocks.filter(f)

  override def parentBlock(m: Block): Option[Block] =
    blocks.find(_ == m).flatMap(block => blocks.find(_.id == block.parentId))

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

  override def continuationIds(info: BifrostSyncInfo, size: Int): ModifierIds = throw new NotImplementedError()

  override def compare(other: BifrostSyncInfo): GenericHistory.HistoryComparisonResult =
    throw new NotImplementedError()

  override def extendsKnownTine(modifier: Block): Boolean = true

  override def idAtHeightOf(height: Long): Option[ModifierId] =
    blocks.find(_.height == height).map(_.id)

  override def getIdsFrom(startBlock: Block, until: Block => Boolean, limit: Int): Option[Seq[ModifierId]] =
    blocks
      .reverse
      .takeWhile(_ != startBlock)
      .reverse
      .takeWhile(block => !until(block))
      .take(limit)
      .map(_.id)
      .some
}

object MockImmutableBlockHistory {
  def apply(blocks: List[Block]): GenericHistory[Block, BifrostSyncInfo, History] = new MockImmutableBlockHistory(blocks)

  def empty: GenericHistory[Block, BifrostSyncInfo, History] = new MockImmutableBlockHistory(List.empty)
}
