package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.ModifierId
import bifrost.modifier.block.{Block, BlockSerializer}
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.settings.AppSettings
import bifrost.utils.Logging
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.hash.Sha256

// fixme: JAA 0 2020.07.19 - why is protobuf still used here?
import serializer.BloomTopics

import scala.collection.BitSet
import scala.concurrent.duration.MILLISECONDS
import scala.util.{Failure, Try}

class Storage(val storage: LSMStore, val settings: AppSettings) extends Logging {
  /* ------------------------------- Cache Initialization ------------------------------- */
  private val cacheExpire: Int = settings.cacheExpire
  private val cacheSize: Int = settings.cacheSize
  type KEY = ByteArrayWrapper
  type VAL = ByteArrayWrapper

  private val blockLoader: CacheLoader[KEY, Option[VAL]] = new CacheLoader[KEY, Option[VAL]] {

    def load(key: KEY): Option[VAL] = {
      storage.get(key) match {
        case Some(blockData: VAL) => Some(blockData)
        case _                    => None
      }
    }
  }

  val blockCache: LoadingCache[KEY, Option[VAL]] = CacheBuilder
    .newBuilder()
    .expireAfterAccess(cacheExpire, MILLISECONDS)
    .maximumSize(cacheSize)
    .build[KEY, Option[VAL]](blockLoader)
  /* ------------------------------------------------------------------------------------- */

  private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))

  def chainHeight: Long = heightOf(bestBlockId).getOrElse(0L)

  def idAtHeight(height: Long): ModifierId = idHeightOf(height).get

  def bestBlockId: ModifierId = blockCache
    .get(bestBlockIdKey)
    .map(d => ModifierId(d.data))
    .getOrElse(ModifierId(History.GenesisParentId))

  def bestChainScore: Long = scoreOf(bestBlockId).get

  def bestBlock: Block = {
    require(chainHeight > 0, "History is empty")
    modifierById(bestBlockId).get
  }

  def modifierById(blockId: ModifierId): Option[Block] = {
    blockCache
      .get(ByteArrayWrapper(blockId.hashBytes))
      .flatMap { bw =>
        val bytes = bw.data
        bytes.head match {
          case Block.modifierTypeId =>
            val parsed = BlockSerializer.parseBytes(bytes.tail)
            parsed match {
              case Failure(e) =>
                log.warn("Failed to parse bytes from db", e)
              case _ =>
            }
            parsed.toOption
          case _ => None
        }
      }
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
  def update(b: Block, diff: Long, isBest: Boolean) {
    log.debug(s"Write new best=$isBest block ${b.id}")
    val typeByte = Block.modifierTypeId

    val blockK: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(ByteArrayWrapper(b.serializedId) -> ByteArrayWrapper(typeByte +: b.bytes))

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val idHeight: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(idHeightKey(parentHeight(b) + 1) → ByteArrayWrapper(b.id.hashBytes))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockDiffKey(b.serializedId) -> ByteArrayWrapper(Longs.toByteArray(diff)))

    // reference Bifrost #519 & #527 for discussion on this division of the score
    val blockScore: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockScoreKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentChainScore(b) + diff / 10000000000L)))

    val bestBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(bestBlockIdKey -> ByteArrayWrapper(b.serializedId))

    val parentBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      if (b.parentId.hashBytes sameElements History.GenesisParentId) {
        Seq()
      } else {
        Seq(blockParentKey(b.serializedId) -> ByteArrayWrapper(b.serializedParentId))
      }

    val blockBloom: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockBloomKey(b.serializedId) -> ByteArrayWrapper(Block.createBloom(b.txs)))

    val newTransactionsToBlockIds: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b.transactions.get.map(tx =>
      (ByteArrayWrapper(tx.serializedId), ByteArrayWrapper(GenericTransaction.modifierTypeId +: b.serializedId))
    )

    /* update storage */
    storage.update(
      ByteArrayWrapper(b.serializedId),
      Seq(),
      blockK ++ blockDiff ++ blockH ++ idHeight ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock
    )

    /* update the cache the in the same way */
    (blockK ++ blockDiff ++ blockH ++ idHeight ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock)
      .foreach(key => blockCache.put(key._1, Some(key._2)))
  }

  /** rollback storage to have the parent block as the last block
    *
    * @param parentId is the parent id of the block intended to be removed
    */
  def rollback(parentId: ModifierId): Try[Unit] = Try {
    blockCache.invalidateAll()
    storage.rollback(ByteArrayWrapper(parentId.hashBytes))
  }

  private def blockScoreKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("score".getBytes ++ blockId.hashBytes))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("height".getBytes ++ blockId.hashBytes))

  private def idHeightKey(height: Long): ByteArrayWrapper =
    ByteArrayWrapper(Sha256(Longs.toByteArray(height)))

  private def blockDiffKey(blockId: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("difficulty".getBytes ++ blockId))

  private def blockParentKey(blockId: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("parentId".getBytes ++ blockId))

  def blockTimestampKey: ByteArrayWrapper =
    ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes))

  private def blockBloomKey(blockId: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("bloom".getBytes ++ blockId))

  def scoreOf(blockId: ModifierId): Option[Long] =
    blockCache
      .get(blockScoreKey(blockId))
      .map(b => Longs.fromByteArray(b.data))

  def heightOf(blockId: ModifierId): Option[Long] =
    blockCache
      .get(blockHeightKey(blockId))
      .map(b => Longs.fromByteArray(b.data))

  def idHeightOf(height: Long): Option[ModifierId] = {
    blockCache
      .get(idHeightKey(height))
      .map(id ⇒ ModifierId(id.data))
  }

  def difficultyOf(blockId: ModifierId): Option[Long] =
    if (blockId.hashBytes sameElements History.GenesisParentId) {
      Some(settings.forgingSettings.InitialDifficulty)
    } else {
      blockCache
        .get(blockDiffKey(blockId.hashBytes))
        .map(b => Longs.fromByteArray(b.data))
    }

  def bloomOf(serializedBlockId: Array[Byte]): Option[BitSet] =
    blockCache
      .get(blockBloomKey(serializedBlockId))
      .map(b => { BitSet() ++ BloomTopics.parseFrom(b.data).topics })

  def serializedParentIdOf(serializedBlockId: Array[Byte]): Option[Array[Byte]] =
    blockCache
      .get(blockParentKey(serializedBlockId))
      .map(d => d.data)

  def blockIdOf(transactionId: Array[Byte]): Option[Array[Byte]] =
    blockCache
      .get(ByteArrayWrapper(transactionId))
      .map(_.data)

  def parentChainScore(b: Block): Long = scoreOf(b.parentId).getOrElse(0L)

  def parentHeight(b: Block): Long = heightOf(b.parentId).getOrElse(0L)

  def parentDifficulty(b: Block): Long = difficultyOf(b.parentId).getOrElse(0L)

  def isGenesis(b: Block): Boolean = b.parentId.hashBytes sameElements History.GenesisParentId
}
