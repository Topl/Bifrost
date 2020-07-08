package bifrost.mempool

import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.nodeView.NodeViewModifier.ModifierId
import bifrost.utils.Logging
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.concurrent.TrieMap
import scala.util.Try


case class MemPool(unconfirmed: TrieMap[ByteArrayWrapper, Transaction])
  extends MemoryPool[Transaction, MemPool] with Logging {
  override type NVCT = MemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  private val boxesInMempool = new TrieMap[ByteArrayWrapper, ByteArrayWrapper]()

  //getters
  override def getById(id: ModifierId): Option[Transaction] = unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[Transaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: Transaction): Try[MemPool] = Try {
    unconfirmed.put(key(tx.id), tx)
    tx.boxIdsToOpen.foreach(boxId => {
      val exists = boxesInMempool.contains(key(boxId))
      require(!exists)
    })
    tx.boxIdsToOpen.foreach(boxId => {
      boxesInMempool.put(key(boxId), key(boxId))
    })
    this
  }

  override def put(txs: Iterable[Transaction]): Try[MemPool] = Try {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    txs.foreach(tx => tx.boxIdsToOpen.foreach(boxId => {
      val exists = boxesInMempool.contains(key(boxId))
      require(!exists)
    }))
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => {
        boxesInMempool.put(key(boxId), key(boxId))
      })
    })
    this
  }

  override def putWithoutCheck(txs: Iterable[Transaction]): MemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => {
        boxesInMempool.put(key(boxId), key(boxId))
      })
    })
    this
  }

  override def remove(tx: Transaction): MemPool = {
    unconfirmed.remove(key(tx.id))
    this
  }

  override def take(limit: Int): Iterable[Transaction] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  override def filter(condition: Transaction => Boolean): MemPool = {
    unconfirmed.retain { (_, v) =>
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


object MemPool {
  lazy val emptyPool: MemPool = MemPool(TrieMap())
}