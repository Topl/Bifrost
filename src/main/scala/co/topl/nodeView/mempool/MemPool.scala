package co.topl.nodeView.mempool

import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.BoxId
import co.topl.utils.Logging

import scala.collection.concurrent.TrieMap
import scala.util.Try


case class MemPool(unconfirmed: TrieMap[ModifierId, Transaction])
  extends MemoryPool[Transaction, MemPool] with Logging {

  override type NVCT = MemPool

  //private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  private val boxesInMempool = new TrieMap[BoxId, BoxId]()

  //getters
  override def modifierById(id: ModifierId): Option[Transaction] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[Transaction] = ids.flatMap(modifierById)

  //modifiers
  override def put(tx: Transaction): Try[MemPool] = Try {
    unconfirmed.put(tx.id, tx)
    tx.boxIdsToOpen.foreach(boxId => require(!boxesInMempool.contains(boxId)))
    tx.boxIdsToOpen.foreach(boxId => boxesInMempool.put(boxId, boxId))
    this
  }

  override def put(txs: Iterable[Transaction]): Try[MemPool] = Try {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    txs.foreach(tx => tx.boxIdsToOpen.foreach {
      boxId => require(!boxesInMempool.contains(boxId))
    })
    txs.foreach(tx => tx.boxIdsToOpen.map {
      boxId => boxesInMempool.put(boxId, boxId)
    })
    this
  }

  override def putWithoutCheck(txs: Iterable[Transaction]): MemPool = {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    txs.foreach(tx => tx.boxIdsToOpen.map {
      boxId => boxesInMempool.put(boxId, boxId)
    })
    this
  }

  override def remove(tx: Transaction): MemPool = {
    unconfirmed.remove(tx.id)
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
          boxesInMempool -= (boxId: BoxId)
        })
        false
      }
    }
    this
  }

  override def size: Int = unconfirmed.size

  override def modifierById(modifierId: ModifierId): Option[Transaction] = {
    unconfirmed.get(modifierId)
  }
}


object MemPool {
  lazy val emptyPool: MemPool = MemPool(TrieMap())
}