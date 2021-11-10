package co.topl.nodeView.history

import co.topl.crypto.hash.blake2b256
import co.topl.crypto.hash.digest.Digest32
import co.topl.crypto.hash.digest.implicits._
import co.topl.modifier.ModifierId
import co.topl.modifier.block.{Block, BloomFilter}
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.KeyValueStore
import co.topl.utils.Logging
import co.topl.codecs.binary._
import com.google.common.primitives.Longs

import scala.util.Try

class Storage(
  private[nodeView] val keyValueStore: KeyValueStore,
  keySize:                             Int
) extends Logging {

  private val bestBlockIdKey = Array.fill(keySize)(-1: Byte)

  def scoreAt(b: ModifierId): Long = scoreOf(b).getOrElse(0L)

  def heightAt(b: ModifierId): Long = heightOf(b).getOrElse(0L)

  def difficultyAt(b: ModifierId): Long = difficultyOf(b).getOrElse(0L)

  def bestBlockId: ModifierId =
    keyValueStore
      .get(bestBlockIdKey)
      .flatMap(d => d.decodePersisted[ModifierId].toOption)
      .getOrElse(History.GenesisParentId)

  def bestBlock: Block =
    modifierById(bestBlockId).getOrElse(throw new Error("Unable to retrieve best block from storage"))

  /** Check for the existence of a modifier in storage without parsing the bytes */
  def containsModifier(id: ModifierId): Boolean =
    keyValueStore.get(id.getIdBytes).isDefined

  /** Retrieve a transaction and its block details from storage */
  def lookupConfirmedTransaction(id: ModifierId): Option[(Transaction.TX, ModifierId, Long)] =
    id.getModType match {
      case Transaction.modifierTypeId =>
        keyValueStore
          .get(id.getIdBytes)
          .flatMap(keyValueStore.get)
          .flatMap(bwBlock => bwBlock.decodePersisted[Block].toOption)
          .map(block => (block.transactions.find(_.id == id).get, block.id, block.height))

      case _ => None
    }

  /** Retrieve a block from storage */
  def modifierById(id: ModifierId): Option[Block] =
    id.getModType match {
      case Block.modifierTypeId =>
        keyValueStore
          .get(id.getIdBytes)
          .flatMap(bwBlock => bwBlock.decodePersisted[Block].toOption)

      case _ =>
        //
        None
    }

  /** These methods allow us to lookup top-level information from blocks using the special keys defined below */
  def scoreOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockScoreKey(blockId))
      .map(b => Longs.fromByteArray(b))

  def heightOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockHeightKey(blockId))
      .map(b => Longs.fromByteArray(b))

  def timestampOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockTimestampKey(blockId))
      .map(b => Longs.fromByteArray(b))

  def idAtHeightOf(height: Long): Option[ModifierId] =
    keyValueStore
      .get(idHeightKey(height).value)
      .flatMap(id => id.decodePersisted[ModifierId].toOption)

  def difficultyOf(blockId: ModifierId): Option[Long] =
    keyValueStore
      .get(blockDiffKey(blockId))
      .map(b => Longs.fromByteArray(b))

  def bloomOf(blockId: ModifierId): Option[BloomFilter] =
    keyValueStore
      .get(blockBloomKey(blockId))
      .flatMap(b => b.decodePersisted[BloomFilter].toOption)

  def parentIdOf(blockId: ModifierId): Option[ModifierId] =
    keyValueStore
      .get(blockParentKey(blockId))
      .flatMap(d => d.decodePersisted[ModifierId].toOption)

  /**
   * The keys below are used to store top-level information about blocks that we might be interested in
   * without needing to parse the entire block from storage
   */
  private def blockScoreKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("score".getBytes ++ blockId.getIdBytes).value

  private def blockHeightKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("height".getBytes ++ blockId.getIdBytes).value

  private def blockDiffKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("difficulty".getBytes ++ blockId.getIdBytes).value

  private def blockTimestampKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("timestamp".getBytes ++ blockId.getIdBytes).value

  private def blockBloomKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("bloom".getBytes ++ blockId.getIdBytes).value

  private def blockParentKey(blockId: ModifierId): Array[Byte] =
    blake2b256.hash("parentId".getBytes ++ blockId.getIdBytes).value

  private def idHeightKey(height: Long): Digest32 = blake2b256.hash(Longs.toByteArray(height))

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

    val blockK = Seq(b.id.getIdBytes -> b.persistedBytes)

    val bestBlock = if (isBest) Seq(bestBlockIdKey -> b.id.persistedBytes) else Seq()

    val newTransactionsToBlockIds = b.transactions.map(tx => (tx.id.getIdBytes, b.id.getIdBytes))

    val blockH = Seq(blockHeightKey(b.id) -> Longs.toByteArray(heightAt(b.parentId) + 1))

    val idHeight = Seq(idHeightKey(heightAt(b.parentId) + 1).bytes -> b.id.persistedBytes)

    val blockDiff = Seq(blockDiffKey(b.id) -> Longs.toByteArray(b.difficulty))

    val blockTimestamp = Seq(blockTimestampKey(b.id) -> Longs.toByteArray(b.timestamp))

    // reference Bifrost #519 & #527 for discussion on this division of the score
    val blockScore = Seq(
      blockScoreKey(b.id) -> Longs.toByteArray(scoreAt(b.parentId) + b.difficulty / 10000000000L)
    )

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
