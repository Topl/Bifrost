package co.topl.nodeView.history

import co.topl.codecs.binary._
import co.topl.codecs.binary.typeclasses.Persistable
import co.topl.consensus.NxtConsensus
import co.topl.settings.ProtocolConfigurations
import co.topl.crypto.hash.blake2b256
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.KeyValueStore
import co.topl.utils.{Int128, Logging}
import com.google.common.primitives.Longs

import scala.util.Try

class Storage(private[history] val keyValueStore: KeyValueStore) extends Logging {

  /** Lookup the id for the tip of the chain */
  private[history] def bestBlockId: Option[ModifierId] =
    keyValueStore
      .get(bestBlockIdKey)
      .flatMap(d => d.decodePersisted[ModifierId].toOption)

  /** Check for the existence of a modifier in storage without parsing the bytes */
  private[history] def containsModifier(id: ModifierId): Boolean =
    keyValueStore.get(id.getIdBytes).isDefined

  /** Retrieve a transaction and its block details from storage */
  private[history] def lookupConfirmedTransaction(id: ModifierId): Option[(Transaction.TX, ModifierId, Long)] =
    id.getModType match {
      case Transaction.modifierTypeId =>
        keyValueStore
          .get(id.getIdBytes)
          .flatMap(keyValueStore.get)
          // ignore first stored block byte which is the modifier type ID (3)
          .flatMap(bwBlock => bwBlock.tail.decodePersisted[Block].toOption)
          .map(block => (block.transactions.find(_.id == id).get, block.id, block.height))

      case _ => None
    }

  /** Retrieve a block from storage */
  private[history] def modifierById(id: ModifierId): Option[Block] =
    id.getModType match {
      case Block.modifierTypeId =>
        keyValueStore
          .get(id.getIdBytes)
          // ignore first stored block byte which is the modifier type ID (3)
          .flatMap(bwBlock => bwBlock.tail.decodePersisted[Block].toOption)

      case _ => None
    }

  /* << EXAMPLE >>
  For version "b00123123":
  ADD
  {
    "b00123123": "Block" | b,
    "diffb00123123": diff,
    "heightb00123123": parentHeight(b00123123) + 1,
    "scoreb00123123": parentChainScore(b00123123) + diff,
    "bestBlock": b00123123
  }
   */
  private[history] def update(
    block:                    Block,
    protocolConfig:           ProtocolConfigurations.Dion,
    applicableConsensusState: NxtConsensus.State,
    isBest:                   Boolean
  ): Unit = {
    log.debug(s"Write new best=$isBest block ${block.id}")

    // store block data with Modifier Type ID for storage backwards compatibility
    // you must go through the Persistable[NodeViewModifier] in order to get the correct type prefix added
    val blockK = Seq(block.id.getIdBytes -> Persistable[NodeViewModifier].persistedBytes(block))

    val bestBlock =
      if (isBest) Seq(bestBlockIdKey -> block.id.persistedBytes) else Seq()

    val newTransactionsToBlockIds = block.transactions.map(tx => (tx.id.getIdBytes, block.id.getIdBytes))

    val blockHeight = Seq(blockHeightKey(block.id) -> block.height.persistedBytes)

    val heightToId = Seq(idHeightKey(block.height) -> block.id.persistedBytes)

    val blockDiff = Seq(blockDiffKey(block.id) -> block.difficulty.persistedBytes)

    val blockTimestamp = Seq(blockTimestampKey(block.id) -> block.timestamp.persistedBytes)

    val blockTotalStake = {
      val newTotalStake = applicableConsensusState.totalStake + applicableConsensusState.inflation
      Seq(totalStakeKey(block.id) -> newTotalStake.persistedBytes)
    }

    val blockInflation = Seq(inflationKey(block.id) -> protocolConfig.inflationRate.persistedBytes)

    // reference Bifrost #519 & #527 for discussion on this division of the score
    val blockScore = {
      val parentScore =
        if (block.parentId == History.GenesisParentId) 0L
        else
          scoreOf(block.parentId).getOrElse(throw new Exception(s"Failed to retrieve score for id: ${block.parentId}"))
      val newBlockScore = parentScore + block.difficulty / 10000000000L
      Seq(blockScoreKey(block.id) -> newBlockScore.persistedBytes)
    }

    val parentBlock =
      if (block.parentId == History.GenesisParentId) Seq()
      else Seq(blockParentKey(block.id) -> block.parentId.persistedBytes)

    val blockBloom = Seq(blockBloomKey(block.id) -> block.bloomFilter.persistedBytes)

    val wrappedUpdate =
      blockK ++
      bestBlock ++
      newTransactionsToBlockIds ++
      blockHeight ++
      heightToId ++
      blockDiff ++
      blockTimestamp ++
      blockTotalStake ++
      blockInflation ++
      blockScore ++
      blockBloom ++
      parentBlock

    /* update storage */
    keyValueStore.update(block.id.persistedBytes, Seq(), wrappedUpdate)

  }

  /**
   * rollback storage to have the parent block as the last block
   *
   * @param parentId is the parent id of the block intended to be removed
   */
  private[history] def rollback(parentId: ModifierId): Try[Unit] = Try {
    keyValueStore.rollbackTo(parentId.persistedBytes)
  }

  /** These methods allow us to lookup top-level information from blocks using the special keys defined below */
  private[history] def scoreOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockScoreKey(blockId))
      .flatMap(b => b.decodePersisted[Long].toOption)

  private[history] def heightOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockHeightKey(blockId))
      .flatMap(b => b.decodePersisted[Long].toOption)

  private[history] def difficultyOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockDiffKey(blockId))
      .flatMap(b => b.decodePersisted[Long].toOption)

  private[history] def timestampOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockTimestampKey(blockId))
      .flatMap(b => b.decodePersisted[Long].toOption)

  private[history] def bloomOf(blockId: ModifierId): Option[BloomFilter] =
    keyValueStore
      .get(blockBloomKey(blockId))
      .flatMap(b => b.decodePersisted[BloomFilter].toOption)

  private[history] def parentIdOf(blockId: ModifierId): Option[ModifierId] =
    keyValueStore
      .get(blockParentKey(blockId))
      .flatMap(d => d.decodePersisted[ModifierId].toOption)

  private[history] def totalStakeOf(blockId: ModifierId): Option[Int128] =
    keyValueStore
      .get(totalStakeKey(blockId))
      .flatMap(d => d.decodePersisted[Int128].toOption)

  private[history] def inflationOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(inflationKey(blockId))
      .flatMap(b => b.decodePersisted[Long].toOption)

  private[history] def idAtHeightOf(height: Long): Option[ModifierId] =
    keyValueStore
      .get(idHeightKey(height))
      .flatMap(id => id.decodePersisted[ModifierId].toOption)

  /**
   * The keys below are used to store top-level information about blocks that we might be interested in
   * without needing to parse the entire block from storage
   */
  private val bestBlockIdKey = Array.fill(33)(-1: Byte)

  private def blockScoreKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("score")(blockId)

  private def blockHeightKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("height")(blockId)

  private def blockDiffKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("difficulty")(blockId)

  private def blockTimestampKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("timestamp")(blockId)

  private def blockBloomKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("bloom")(blockId)

  private def blockParentKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("parentId")(blockId)

  private def totalStakeKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("totalStake")(blockId)

  private def inflationKey(blockId: ModifierId): Array[Byte] = keyToSaveDataByBlockId("inflation")(blockId)

  private def idHeightKey(height: Long): Array[Byte] = blake2b256.hash(Longs.toByteArray(height)).value

  private def keyToSaveDataByBlockId(dataLabel: String): ModifierId => Array[Byte] = (blockId: ModifierId) =>
    blake2b256.hash(dataLabel.getBytes("UTF-8") ++ blockId.getIdBytes).value
}
