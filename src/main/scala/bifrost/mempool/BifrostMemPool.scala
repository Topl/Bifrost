package bifrost.mempool

import bifrost.transaction.BifrostTransaction
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.collection.concurrent.TrieMap
import scala.util.{Failure, Success, Try}


case class BifrostMemPool(unconfirmed: TrieMap[ByteArrayWrapper, BifrostTransaction])
  extends MemoryPool[BifrostTransaction, BifrostMemPool] with ScorexLogging {
  override type NVCT = BifrostMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)
  private val boxesInMempool = new TrieMap[ByteArrayWrapper, ByteArrayWrapper]()

  //getters
  override def getById(id: ModifierId): Option[BifrostTransaction] = unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[BifrostTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: BifrostTransaction): Try[BifrostMemPool] = Try {
    unconfirmed.put(key(tx.id), tx)
    tx.boxIdsToOpen.foreach(boxId => {
      val exists = boxesInMempool.get(key(boxId)).isDefined
      require(!exists)
    })
    tx.boxIdsToOpen.foreach(boxId => {
      boxesInMempool.put(key(boxId), key(boxId))
    })
    this
  }

  override def put(txs: Iterable[BifrostTransaction]): Try[BifrostMemPool] = Try {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    txs.foreach(tx => tx.boxIdsToOpen.foreach(boxId => {
      val exists = boxesInMempool.get(key(boxId)).isDefined
      require(!exists)
    }))
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => { boxesInMempool.put(key(boxId), key(boxId)) })
    })
    this
  }

  override def putWithoutCheck(txs: Iterable[BifrostTransaction]): BifrostMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => { boxesInMempool.put(key(boxId), key(boxId)) })
    })
    this
  }

  override def remove(tx: BifrostTransaction): BifrostMemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[BifrostTransaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: (BifrostTransaction) => Boolean): BifrostMemPool = {
    unconfirmed.retain { (k, v) =>
      if (condition(v)) {
        true
      } else {
        v.boxIdsToOpen.foreach(boxId => {
          boxesInMempool -= (key(boxId): ByteArrayWrapper)
        })
        false
      }
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object BifrostMemPool {
  lazy val emptyPool: BifrostMemPool = BifrostMemPool(TrieMap())
}