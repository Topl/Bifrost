package bifrost.history

import bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import bifrost.forging.ForgingSettings
import com.typesafe.config.Config
import com.google.common.primitives.Longs
import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.NodeViewModifier._
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.transaction.Transaction
import bifrost.utils.ScorexLogging
import scorex.crypto.hash.Sha256
import serializer.BloomTopics

import scala.collection.BitSet
import scala.util.{Failure, Try}
import scala.concurrent.duration.MILLISECONDS
import com.typesafe.config.ConfigFactory

class BifrostStorage(val storage: LSMStore, val settings: ForgingSettings) extends ScorexLogging {
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

  def bestBlockId: Array[Byte] = blockCache
    .get(bestBlockIdKey)
    .map(_.data)
    .getOrElse(settings.GenesisParentId)

  def bestChainScore: Long = scoreOf(bestBlockId).get

  def bestBlock: BifrostBlock = {
    require(height > 0, "History is empty")
    modifierById(bestBlockId).get
  }

  def modifierById(blockId: ModifierId): Option[BifrostBlock] = {
    blockCache
      .get(ByteArrayWrapper(blockId))
      .flatMap { bw =>
        val bytes = bw.data
        bytes.head match {
          case BifrostBlock.ModifierTypeId =>
            val parsed = {
              heightOf(blockId) match {
                case Some(x) if x <= settings.forkHeight => BifrostBlockCompanion.parseBytes2xAndBefore(bytes.tail)
                case _ => BifrostBlockCompanion.parseBytes(bytes.tail)
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
        "b00123123": "BifrostBlock" | b,
        "diffb00123123": diff,
        "heightb00123123": parentHeight(b00123123) + 1,
        "scoreb00123123": parentChainScore(b00123123) + diff,
        "bestBlock": b00123123
      }
   */
  def update(b: BifrostBlock, diff: Long, isBest: Boolean) {
    log.debug(s"Write new best=$isBest block ${b.encodedId}")
    val typeByte = BifrostBlock.ModifierTypeId

    val blockK: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes))

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockDiffKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(diff)))

    val blockScore: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockScoreKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentChainScore(b) + diff)))

    val bestBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(bestBlockIdKey -> ByteArrayWrapper(b.id))

    val parentBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      (b.parentId sameElements settings.GenesisParentId) match {
        case true => Seq()
        case false => Seq(blockParentKey(b.id) -> ByteArrayWrapper(b.parentId))
      }

    val blockBloom: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockBloomKey(b.id) -> ByteArrayWrapper(BifrostBlock.createBloom(b.txs)))

    val newTransactionsToBlockIds: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b.transactions.get.map(
      tx => (ByteArrayWrapper(tx.id), ByteArrayWrapper(Transaction.ModifierTypeId +: b.id))
    )

    /* update storage */
    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      blockK ++ blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock
    )

    /* update the cache the in the same way */
    (blockK ++ blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock)
      .foreach(key => blockCache.put(key._1, Some(key._2)))
  }

  /** rollback storage to have the parent block as the last block
    *
    * @param parentBlockId is the parent id of the block intended to be removed
    */
  def rollback(parentBlockId: ModifierId): Try[Unit] = Try {
    storage.rollback(ByteArrayWrapper(parentBlockId))
  }

  /** Invalidate the entries related to the block that gets rolled back, this happens when history calls drop()
    *
    * @param blockId  is the id of the block intended to be removed
    * @param parentId is the parent id of the block intended to be removed
    */
  def cacheBack(blockId: ModifierId, parentId: ModifierId) {
    val blockK: Iterable[ByteArrayWrapper] = Seq(ByteArrayWrapper(blockId))
    val blockH: Iterable[ByteArrayWrapper] = Seq(blockHeightKey(blockId))
    val blockDiff: Iterable[ByteArrayWrapper] = Seq(blockDiffKey(blockId))
    val blockScore: Iterable[ByteArrayWrapper] = Seq(blockScoreKey(blockId))
    val bestBlock: Iterable[ByteArrayWrapper] = Seq(bestBlockIdKey)
    val blockBloom: Iterable[ByteArrayWrapper] = Seq(blockBloomKey(blockId))

    val parentBlock: Iterable[ByteArrayWrapper] = (parentId sameElements settings.GenesisParentId) match {
      case true => Seq()
      case false => Seq(blockParentKey(blockId))
    }

    val newTransactionsToBlockIds: Iterable[ByteArrayWrapper] = modifierById(blockId).get.transactions.get.map(
      tx => ByteArrayWrapper(tx.id)
    )

    (blockK ++ blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock)
      .foreach(key => blockCache.invalidate(key))
  }

  private def blockScoreKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("score".getBytes ++ blockId))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("height".getBytes ++ blockId))

  private def blockDiffKey(blockId: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("difficulty".getBytes ++ blockId))

  private def blockParentKey(blockId: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(Sha256("parentId"
    .getBytes ++ blockId))

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
        .get(blockDiffKey(blockId))
        .map(b => Longs.fromByteArray(b.data))
    }

  def bloomOf(blockId: ModifierId): Option[BitSet] =
    blockCache
      .get(blockBloomKey(blockId))
      .map(b => {BitSet() ++ BloomTopics.parseFrom(b.data).topics})

  def parentIdOf(blockId: ModifierId): Option[ModifierId] =
    blockCache
      .get(blockParentKey(blockId))
      .map(_.data)

  def blockIdOf(transactionId: ModifierId): Option[Array[Byte]] =
    blockCache
      .get(ByteArrayWrapper(transactionId))
      .map(_.data)

  def parentChainScore(b: BifrostBlock): Long = scoreOf(b.parentId).getOrElse(0L)

  def parentHeight(b: BifrostBlock): Long = heightOf(b.parentId).getOrElse(0L)

  def parentDifficulty(b: BifrostBlock): Long = difficultyOf(b.parentId).getOrElse(0L)

  def isGenesis(b: BifrostBlock): Boolean = b.parentId sameElements settings.GenesisParentId
}