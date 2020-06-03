package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.forging.ForgingSettings
import bifrost.modifier.block.{Block, BlockCompanion}
import bifrost.modifier.transaction.bifrostTransaction.GenericTransaction
import bifrost.nodeView.NodeViewModifier._
import bifrost.utils.{Logging, bytesToId, idToBytes}
import com.google.common.cache.{CacheBuilder, CacheLoader}
import com.google.common.primitives.Longs
import com.typesafe.config.{Config, ConfigFactory}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.hash.Sha256
import serializer.BloomTopics

import scala.collection.BitSet
import scala.concurrent.duration.MILLISECONDS
import scala.util.{Failure, Try}

class Storage(val storage: LSMStore, val settings: ForgingSettings) extends Logging {
  /* ------------------------------- Cache Initialization ------------------------------- */
  private val conf: Config = ConfigFactory.load("application")
  private val expireTime: Int = conf.getInt("cache.expireTime")
  private val cacheSize: Int = conf.getInt("cache.cacheSize")
  type KEY = ByteArrayWrapper
  type VAL = ByteArrayWrapper

  private val blockLoader: CacheLoader[KEY, Option[VAL]] = new CacheLoader[KEY, Option[VAL]] {
    def load(key: KEY): Option[VAL] = {
      storage.get(key) match {
        case Some(blockData: VAL) => Some(blockData)
        case _ => None
      }
    }
  }

  val blockCache = CacheBuilder.newBuilder()
    .expireAfterAccess(expireTime, MILLISECONDS)
    .maximumSize(cacheSize)
    .build[KEY, Option[VAL]](blockLoader)
  /* ------------------------------------------------------------------------------------- */

  private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))

  def height: Long = heightOf(bestBlockId).getOrElse(0L)

  def bestBlockId: ModifierId = blockCache
    .get(bestBlockIdKey)
    .map(d => bytesToId(d.data))
    .getOrElse(settings.GenesisParentId)

  def bestChainScore: Long = scoreOf(bestBlockId).get

  def bestBlock: Block = {
    require(height > 0, "History is empty")
    modifierById(bestBlockId).get
  }

  def modifierById(blockId: ModifierId): Option[Block] = {
    blockCache
      .get(ByteArrayWrapper(idToBytes(blockId)))
      .flatMap { bw =>
        val bytes = bw.data
        bytes.head match {
          case Block.modifierTypeId =>
            val parsed = {
              heightOf(blockId) match {
                case Some(x) if x <= settings.forkHeight => BlockCompanion.parseBytes2xAndBefore(bytes.tail)
                case _ => BlockCompanion.parseBytes(bytes.tail)
              }
            }
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
      Seq(ByteArrayWrapper(idToBytes(b.id)) -> ByteArrayWrapper(typeByte +: b.bytes))

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockDiffKey(idToBytes(b.id)) -> ByteArrayWrapper(Longs.toByteArray(diff)))

    val blockScore: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockScoreKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentChainScore(b) + diff)))

    val bestBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(bestBlockIdKey -> ByteArrayWrapper(idToBytes(b.id)))

    val parentBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      (b.parentId sameElements settings.GenesisParentId) match {
        case true => Seq()
        case false => Seq(blockParentKey(idToBytes(b.id)) -> ByteArrayWrapper(idToBytes(b.parentId)))
      }

    val blockBloom: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockBloomKey(idToBytes(b.id)) -> ByteArrayWrapper(Block.createBloom(b.txs)))

    val newTransactionsToBlockIds: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b.transactions.get.map(
      tx => (ByteArrayWrapper(idToBytes(tx.id)), ByteArrayWrapper(GenericTransaction.ModifierTypeId +: idToBytes(b.id)))
    )

    /* update storage */
    storage.update(
      ByteArrayWrapper(idToBytes(b.id)),
      Seq(),
      blockK ++ blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock
    )

    /* update the cache the in the same way */
    (blockK ++ blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock)
      .foreach(key => blockCache.put(key._1, Some(key._2)))
  }

  /** rollback storage to have the parent block as the last block
    *
    * @param parentId is the parent id of the block intended to be removed
    */
  def rollback(parentId: ModifierId): Try[Unit] = Try {
    blockCache.invalidateAll()
    storage.rollback(ByteArrayWrapper(idToBytes(parentId)))
  }

  private def blockScoreKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("score".getBytes ++ blockId))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("height".getBytes ++ blockId))

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

  def difficultyOf(blockId: ModifierId): Option[Long] =
    if (blockId sameElements settings.GenesisParentId) {
      Some(settings.InitialDifficulty)
    } else {
      blockCache
        .get(blockDiffKey(idToBytes(blockId)))
        .map(b => Longs.fromByteArray(b.data))
    }

  def bloomOf(blockId: ModifierId): Option[BitSet] =
    blockCache
      .get(blockBloomKey(idToBytes(blockId)))
      .map(b => {BitSet() ++ BloomTopics.parseFrom(b.data).topics})

  def parentIdOf(blockId: ModifierId): Option[ModifierId] =
    blockCache
      .get(blockParentKey(idToBytes(blockId)))
      .map(d => bytesToId(d.data))

  def blockIdOf(transactionId: ModifierId): Option[Array[Byte]] =
    blockCache
      .get(ByteArrayWrapper(idToBytes(transactionId)))
      .map(_.data)

  def parentChainScore(b: Block): Long = scoreOf(b.parentId).getOrElse(0L)

  def parentHeight(b: Block): Long = heightOf(b.parentId).getOrElse(0L)

  def parentDifficulty(b: Block): Long = difficultyOf(b.parentId).getOrElse(0L)

  def isGenesis(b: Block): Boolean = b.parentId sameElements settings.GenesisParentId
}