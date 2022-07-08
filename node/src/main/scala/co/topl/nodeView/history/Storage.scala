package co.topl.nodeView.history

import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.digest.implicits._
import co.topl.modifier.{ModifierId, NodeViewModifier}
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.KeyValueStore
import co.topl.utils.Logging
import co.topl.codecs.binary._
import co.topl.codecs.binary.typeclasses.Persistable
import co.topl.modifier
import com.google.common.primitives.Longs

import scala.util.Try

class Storage(private[history] val keyValueStore: KeyValueStore) extends Logging {

  private val bestBlockIdKey = Array.fill(33)(-1: Byte)

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

      case _ =>
        //
        None
    }

  def heightOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockHeightKey(blockId))
      .map(b => Longs.fromByteArray(b))

  /** These methods allow us to lookup top-level information from blocks using the special keys defined below */
  private[history] def scoreOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockScoreKey(blockId))
      .map(b => Longs.fromByteArray(b))

  private[history] def difficultyOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockDiffKey(blockId))
      .map(b => Longs.fromByteArray(b))

  private[history] def timestampOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockTimestampKey(blockId))
      .map(b => Longs.fromByteArray(b))

  private[history] def bloomOf(blockId: ModifierId): Option[BloomFilter] =
    keyValueStore
      .get(blockBloomKey(blockId))
      .flatMap(b => b.decodePersisted[BloomFilter].toOption)

  private[history] def parentIdOf(blockId: ModifierId): Option[ModifierId] =
    keyValueStore
      .get(blockParentKey(blockId))
      .flatMap(d => d.decodePersisted[ModifierId].toOption)

  private[history] def idAtHeightOf(height: Long): Option[ModifierId] =
    keyValueStore
      .get(idHeightKey(height))
      .flatMap(id => id.decodePersisted[ModifierId].toOption)

  /**
   * The keys below are used to store top-level information about blocks that we might be interested in
   * without needing to parse the entire block from storage
   */
  private def blockScoreKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("score".getBytes("UTF-8") ++ blockId.getIdBytes).value

  private def blockHeightKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("height".getBytes("UTF-8") ++ blockId.getIdBytes).value

  private def blockDiffKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("difficulty".getBytes("UTF-8") ++ blockId.getIdBytes).value

  private def blockTimestampKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("timestamp".getBytes("UTF-8") ++ blockId.getIdBytes).value

  private def blockBloomKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("bloom".getBytes("UTF-8") ++ blockId.getIdBytes).value

  private def blockParentKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("parentId".getBytes("UTF-8") ++ blockId.getIdBytes).value

  private def idHeightKey(height: Long): Array[Byte] = blake2b256.hash(Longs.toByteArray(height)).value

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
  def update(b: Block, isBest: Boolean): Unit = {
    log.debug(s"Write new best=$isBest block ${b.id}")

    // store block data with Modifier Type ID for storage backwards compatibility
    val blockK = Seq(b.id.getIdBytes -> b.id.persistedBytes)

    val bestBlock =
      if (isBest) Seq(bestBlockIdKey -> b.id.persistedBytes) else Seq()

    val newTransactionsToBlockIds = b.transactions.map(tx => (tx.id.getIdBytes, b.id.getIdBytes))

    val blockH = Seq(blockHeightKey(b.id) -> Longs.toByteArray(b.height))

    val idHeight = Seq(idHeightKey(b.height) -> b.id.persistedBytes)

    val blockDiff = Seq(blockDiffKey(b.id) -> Longs.toByteArray(b.difficulty))

    val blockTimestamp = Seq(blockTimestampKey(b.id) -> Longs.toByteArray(b.timestamp))

    // reference Bifrost #519 & #527 for discussion on this division of the score
    val blockScore = {
      val parentScore = scoreOf(b.parentId).getOrElse(
        throw new Exception(s"Failed to retrieve score for id: ${b.parentId}")
      )
      Seq(blockScoreKey(b.id) -> Longs.toByteArray(parentScore + b.difficulty / 10000000000L))
    }

    val parentBlock =
      if (b.parentId == History.GenesisParentId) Seq()
      else Seq(blockParentKey(b.id) -> b.parentId.persistedBytes)

    val blockBloom = Seq(blockBloomKey(b.id) -> b.bloomFilter.persistedBytes)

    val wrappedUpdate =
      blockK ++
      blockDiff ++
      blockTimestamp ++
      blockH ++
      idHeight ++
      blockScore ++
      bestBlock ++
      newTransactionsToBlockIds ++
      blockBloom ++
      parentBlock

    /* update storage */
    keyValueStore.update(b.id.persistedBytes, Seq(), wrappedUpdate)

  }

  /**
   * rollback storage to have the parent block as the last block
   *
   * @param parentId is the parent id of the block intended to be removed
   */
  def rollback(parentId: ModifierId): Try[Unit] = Try {
    keyValueStore.rollbackTo(parentId.persistedBytes)
  }
}
