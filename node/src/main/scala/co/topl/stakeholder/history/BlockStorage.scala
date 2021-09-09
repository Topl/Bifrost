package co.topl.stakeholder.history

import com.google.common.cache.{CacheBuilder, CacheLoader, LoadingCache, RemovalNotification}
import co.topl.stakeholder.primitives.ByteArrayWrapper
import co.topl.stakeholder.components.{Block, Serializer}
import co.topl.stakeholder.primitives.{ByteStream, LDBStore, SharedData, SimpleTypes, TetraParameters}
import co.topl.stakeholder.primitives.Base58
import scala.util.Try

/**
 * AMS 2020:
 * Storage for block body and header data stored separately in databases that are split into thirds of epochs
 */

class BlockStorage(dir: String, serializer: Serializer) extends SimpleTypes {
  import co.topl.stakeholder.components.Serializer._
  val cacheSize = TetraParameters.cacheSize
  val one_ninth_epoch = TetraParameters.one_ninth_epoch
  val dbCacheSize = 4
  type DB = LDBStore

  private val headerStoreCache: LoadingCache[BigInt, DB] = CacheBuilder
    .newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener { (notification: RemovalNotification[BigInt, DB]) =>
      notification.getValue.close()
    }
    .build[BigInt, DB](new CacheLoader[BigInt, DB] {

      def load(epoch9th: BigInt): DB =
        LDBStore(s"$dir/blocks/header/epoch_${epoch9th / 9}_${epoch9th % 9}")
    })

  private val bodyStoreCache: LoadingCache[BigInt, DB] = CacheBuilder
    .newBuilder()
    .maximumSize(dbCacheSize)
    .removalListener { (notification: RemovalNotification[BigInt, DB]) =>
      notification.getValue.close()
    }
    .build[BigInt, DB](new CacheLoader[BigInt, DB] {

      def load(epoch9th: BigInt): DB =
        LDBStore(s"$dir/blocks/body/epoch_${epoch9th / 9}_${epoch9th % 9}")
    })

  def refresh(): Unit = {
    bodyStoreCache.asMap().keySet().forEach(bodyStoreCache.get(_).refresh())
    headerStoreCache.asMap().keySet().forEach(headerStoreCache.get(_).refresh())
  }

  private val blockCache: LoadingCache[SlotId, Block] = CacheBuilder
    .newBuilder()
    .maximumSize(cacheSize)
    .build[SlotId, Block](new CacheLoader[SlotId, Block] {

      def load(id: SlotId): Block =
        restoreBlock(id).get
    })

  def add(block: Block): Unit = {
    val blockHeader = block.tetraHeader
    headerStoreCache
      .get(blockHeader._3 / one_ninth_epoch)
      .update(Seq(), Seq(block.id -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache
        .get(blockHeader._3 / one_ninth_epoch)
        .update(
          Seq(),
          Seq(
            block.id -> ByteArrayWrapper(
              serializer.getGenesisBytes(
                block.genesisSet.get
              )
            )
          )
        )
    } else {
      bodyStoreCache
        .get(blockHeader._3 / one_ninth_epoch)
        .update(
          Seq(),
          Seq(
            block.id -> ByteArrayWrapper(
              serializer.getBytes(
                block.blockBody.get
              )
            )
          )
        )
    }
    blockCache.put((blockHeader._3, block.id), block)
  }

  def store(key: ByteArrayWrapper, block: Block): Unit = {
    val blockHeader = block.tetraHeader
    headerStoreCache
      .get(blockHeader._3 / one_ninth_epoch)
      .update(Seq(), Seq(key -> ByteArrayWrapper(serializer.getBytes(blockHeader))))
    if (blockHeader._3 == 0) {
      bodyStoreCache
        .get(blockHeader._3 / one_ninth_epoch)
        .update(
          Seq(),
          Seq(
            key -> ByteArrayWrapper(
              serializer.getGenesisBytes(
                block.genesisSet.get
              )
            )
          )
        )
    } else {
      bodyStoreCache
        .get(blockHeader._3 / one_ninth_epoch)
        .update(
          Seq(),
          Seq(
            key -> ByteArrayWrapper(
              serializer.getBytes(
                block.blockBody.get
              )
            )
          )
        )
    }
  }

  def restoreBlock(id: SlotId): Option[Block] = Try {
    val key = id._2
    SharedData.throwDiskWarning(s"block database ${Base58.encode(key.data)}")
    headerStoreCache.get(id._1 / one_ninth_epoch).get(key) match {
      case Some(bytes: ByteArrayWrapper) =>
        val byteStream: ByteStream = new ByteStream(bytes.data, DeserializeBlockHeader)
        serializer.fromBytes(byteStream) match {
          case h: BlockHeader @unchecked =>
            bodyStoreCache.get(id._1 / one_ninth_epoch).get(key) match {
              case Some(bytes: ByteArrayWrapper) =>
                if (h._3 == 0) {
                  val byteStream: ByteStream = new ByteStream(bytes.data, DeserializeGenesisSet)
                  serializer.fromBytes(byteStream) match {
                    case txs: GenesisSeq @unchecked =>
                      val block = Block(key, Some(h), None, Some(txs))
                      Some(block)
                    case _ => None
                  }
                } else {
                  val byteStream: ByteStream = new ByteStream(bytes.data, DeserializeTransactionSet)
                  serializer.fromBytes(byteStream) match {
                    case txs: TransactionSeq @unchecked =>
                      val block = Block(key, Some(h), Some(txs), None)
                      Some(block)
                    case _ => None
                  }
                }
              case None =>
                val block = Block(key, Some(h), None, None)
                Some(block)
            }
        }
      case None => None
    }
  }.toOption match {
    case Some(value) => value
    case None        => None
  }

  def restoreHeader(id: SlotId): Option[BlockHeader] = Try {
    val key = id._2
    SharedData.throwDiskWarning(s"block database ${Base58.encode(key.data)}")
    headerStoreCache.get(id._1 / one_ninth_epoch).get(key) match {
      case Some(bytes: ByteArrayWrapper) =>
        val byteStream: ByteStream = new ByteStream(bytes.data, DeserializeBlockHeader)
        serializer.fromBytes(byteStream) match {
          case h: BlockHeader @unchecked => Some(h)
        }
      case None => None
    }
  }.toOption match {
    case Some(value) => value
    case None        => None
  }

  def get(id: SlotId): Option[Block] = Try(blockCache.get(id)).toOption

  def getIfPresent(id: SlotId): Option[Block] =
    Try {
      blockCache.getIfPresent(id) match {
        case b: Block => b
      }
    }.toOption match {
      case Some(b: Block) => Some(b)
      case None           => restoreBlock(id)
    }

  def known(id: SlotId): Boolean =
    Try(blockCache.get(id)).toOption match {
      case Some(_: Block) => true
      case None           => false
    }

  def knownIfPresent(id: SlotId): Boolean =
    Try {
      blockCache.getIfPresent(id) match {
        case b: Block => b
      }
    }.toOption match {
      case Some(_: Block) => true
      case None =>
        headerStoreCache.get(id._1 / one_ninth_epoch).known(id._2)
    }

  def knownInCache(id: SlotId): Boolean = blockCache.asMap().keySet().contains(id)

  def getIfInCache(id: SlotId): Option[Block] =
    blockCache.getIfPresent(id) match {
      case b: Block => Some(b)
      case _        => None
    }

  def populateCache(id: SlotId): Unit = {
    var i = 0
    var loadId = id
    var done = false
    while (i < cacheSize && !done) {
      blockCache.refresh(loadId)
      if (loadId._1 > 0) {
        loadId = blockCache.get(loadId).parentSlotId
        i += 1
      } else {
        done = true
      }
    }
  }

}
