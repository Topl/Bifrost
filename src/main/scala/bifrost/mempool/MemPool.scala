package bifrost.mempool

import bifrost.modifier.ModifierId
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.utils.Logging
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.concurrent.TrieMap
import scala.util.Try

case class MemPool(unconfirmed: TrieMap[ByteArrayWrapper, Transaction]) extends MemoryPool[Transaction, MemPool] with Logging {
  override type NVCT = MemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  private val boxesInMempool = new TrieMap[ByteArrayWrapper, ByteArrayWrapper]()

  //getters
  override def getById(id: ModifierId): Option[Transaction] = unconfirmed.get(key(id.hashBytes))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id.hashBytes))

  override def getAll(ids: Seq[ModifierId]): Seq[Transaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: Transaction): Try[MemPool] = Try {
    unconfirmed.put(key(tx.id.hashBytes), tx)
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
    txs.foreach(tx => unconfirmed.put(key(tx.id.hashBytes), tx))
    txs.foreach(tx =>
      tx.boxIdsToOpen.foreach(boxId => {
        val exists = boxesInMempool.contains(key(boxId))
        require(!exists)
      })
    )
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => {
        boxesInMempool.put(key(boxId), key(boxId))
      })
    })
    this
  }

  override def putWithoutCheck(txs: Iterable[Transaction]): MemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id.hashBytes), tx))
    txs.foreach(tx => {
      tx.boxIdsToOpen.map(boxId => {
        boxesInMempool.put(key(boxId), key(boxId))
      })
    })
    this
  }

  override def remove(tx: Transaction): MemPool = {
    unconfirmed.remove(key(tx.id.hashBytes))
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

  override def modifierById(modifierId: ModifierId): Option[Transaction] = {
    unconfirmed.get(ByteArrayWrapper(modifierId.hashBytes))
  }
}

object MemPool {
  lazy val emptyPool: MemPool = MemPool(TrieMap())
}
