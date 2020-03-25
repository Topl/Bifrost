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
import scala.concurrent.duration.MINUTES
import com.typesafe.config.ConfigFactory

class BifrostStorage(val storage: LSMStore, val settings: ForgingSettings) extends ScorexLogging {
  private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))
  // --------------------------------------------------------------------------------------
  // ---------------------------Initialize cache-------------------------------------------
  val conf: Config = ConfigFactory.load("application")
  val expireTime: Int = conf.getInt("cache.expireTime")
  val cacheSize: Int = conf.getInt("cache.cacheSize")
  val cacheOrNot: Boolean = conf.getBoolean("cache.cacheOrNot")
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

  private val blockCache = CacheBuilder.newBuilder()
    .expireAfterAccess(expireTime,MINUTES)
    .maximumSize(cacheSize)
    .build[KEY, Option[VAL]](blockLoader)
  // --------------------------------------------------------------------------------------
  // --------------------------------------------------------------------------------------

  def height: Long = heightOf(bestBlockId).getOrElse(0L)

  def bestBlockId: Array[Byte] = (if (cacheOrNot) blockCache.get(bestBlockIdKey) else storage.get(bestBlockIdKey))
    .map(_.data)
    .getOrElse(settings.GenesisParentId)

  def bestChainScore: Long = scoreOf(bestBlockId).get

  def bestBlock: BifrostBlock = {
    require(height > 0, "History is empty")
    modifierById(bestBlockId).get
  }

  def modifierById(blockId: ModifierId): Option[BifrostBlock] = {
    (if (cacheOrNot) blockCache.get(ByteArrayWrapper(blockId)) else storage.get(ByteArrayWrapper(blockId)))
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

  def update(b: BifrostBlock, diff: Long, isBest: Boolean) {
    log.debug(s"Write new best=$isBest block ${b.encodedId}")
    val typeByte = BifrostBlock.ModifierTypeId

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockDiffKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(diff)))

    val blockScore: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockScoreKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentChainScore(b) + diff)))

    val bestBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(bestBlockIdKey -> ByteArrayWrapper(b.id))

    val parentBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = {
      if (b.parentId sameElements settings.GenesisParentId) {
        Seq()
      } else {
        Seq(blockParentKey(b.id) -> ByteArrayWrapper(b.parentId))
      }
    }

    val blockBloom: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockBloomKey(b.id) -> ByteArrayWrapper(BifrostBlock.createBloom(b.txs)))

    val newTransactionsToBlockIds: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = b.transactions.get.map(
      tx => (ByteArrayWrapper(tx.id), ByteArrayWrapper(Transaction.ModifierTypeId +: b.id))
    )

    /* << EXAMPLE >>
      For version "b00123123":
      ADD
      {
        "diffb00123123": diff,
        "heightb00123123": parentHeight(b00123123) + 1,
        "scoreb00123123": parentChainScore(b00123123) + diff,
        "bestBlock": b00123123,
        "b00123123": "BifrostBlock" | b
      }
    */
    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock ++
        Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes))
    )

    // update the cache the in the same way
    (blockDiff ++ blockH ++ blockScore ++ bestBlock ++ newTransactionsToBlockIds ++ blockBloom ++ parentBlock ++
      Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
      .foreach(key => blockCache.put(key._1, Some(key._2)))
  }

  def rollback(modifierId: ModifierId): Try[Unit] = Try {
    storage.rollback(ByteArrayWrapper(modifierId))
    // not flushing the cache and letting them expire
    // blockCache.invalidateAll()
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
    (if (cacheOrNot) blockCache.get(blockScoreKey(blockId)) else storage.get(blockScoreKey(blockId)))
      .map(b => Longs.fromByteArray(b.data))

  def heightOf(blockId: ModifierId): Option[Long] =
    (if (cacheOrNot) blockCache.get(blockHeightKey(blockId)) else storage.get(blockHeightKey(blockId)))
      .map(b => Longs.fromByteArray(b.data))

  def difficultyOf(blockId: ModifierId): Option[Long] = if (blockId sameElements settings.GenesisParentId) {
    Some(settings.InitialDifficulty)
  } else {
    (if (cacheOrNot) blockCache.get(blockDiffKey(blockId)) else storage.get(blockDiffKey(blockId)))
      .map(b => Longs.fromByteArray(b.data))
  }

  def bloomOf(blockId: ModifierId): Option[BitSet] =
    (if (cacheOrNot) blockCache.get(blockBloomKey(blockId)) else storage.get(blockBloomKey(blockId)))
      .map(b => {BitSet() ++ BloomTopics.parseFrom(b.data).topics})

  def parentIdOf(blockId: ModifierId): Option[ModifierId] =
    (if (cacheOrNot) blockCache.get(blockParentKey(blockId)) else storage.get(blockParentKey(blockId)))
      .map(_.data)

  def blockIdOf(transactionId: ModifierId): Option[Array[Byte]] =
    (if (cacheOrNot) blockCache.get(ByteArrayWrapper(transactionId)) else storage.get(ByteArrayWrapper(transactionId)))
      .map(_.data)

  def parentChainScore(b: BifrostBlock): Long = scoreOf(b.parentId).getOrElse(0L)

  def parentHeight(b: BifrostBlock): Long = heightOf(b.parentId).getOrElse(0L)

  def parentDifficulty(b: BifrostBlock): Long = difficultyOf(b.parentId).getOrElse(0L)

  def isGenesis(b: BifrostBlock): Boolean = b.parentId sameElements settings.GenesisParentId

}