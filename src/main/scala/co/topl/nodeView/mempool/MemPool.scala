package co.topl.nodeView.mempool

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.{ Box, BoxId }
import co.topl.utils.Logging

import scala.collection.concurrent.TrieMap
import scala.util.Try

case class MemPool[TX <: Transaction[_, Proposition, Proof[_], Box[_]]](unconfirmed: TrieMap[ModifierId, TX])
  extends MemoryPool[TX, MemPool[TX]] with Logging {

  override type NVCT = MemPool[TX]

  

  //private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  private val boxesInMempool = new TrieMap[BoxId, BoxId]()

  //getters
  override def modifierById(id: ModifierId): Option[TX] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[TX] = ids.flatMap(modifierById)

  override def size: Int = unconfirmed.size

  //modifiers
  override def put(tx: TX): Try[MemPool[TX]] = Try {
    unconfirmed.put(tx.id, tx)
    tx.boxIdsToOpen.foreach(boxId => require(!boxesInMempool.contains(boxId)))
    tx.boxIdsToOpen.foreach(boxId => boxesInMempool.put(boxId, boxId))
    this
  }

  /**
   *
   * @param txs
   * @return
   */
  override def put(txs: Iterable[TX]): Try[MemPool[TX]] = Try {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    txs.foreach(tx => tx.boxIdsToOpen.foreach {
      boxId => require(!boxesInMempool.contains(boxId))
    })
    txs.foreach(tx => tx.boxIdsToOpen.map {
      boxId => boxesInMempool.put(boxId, boxId)
    })
    this
  }

  /**
   *
   * @param txs
   * @return
   */
  override def putWithoutCheck(txs: Iterable[TX]): MemPool[TX] = {
    txs.foreach(tx => unconfirmed.put(tx.id, tx))
    txs.foreach(tx => tx.boxIdsToOpen.map {
      boxId => boxesInMempool.put(boxId, boxId)
    })
    this
  }

  /**
   *
   * @param tx
   * @return
   */
  override def remove(tx: TX): MemPool[TX] = {
    unconfirmed.remove(tx.id)
    this
  }

  /**
   *
   * @param limit
   * @return
   */
  override def take(limit: Int): Iterable[TX] =
    unconfirmed.values.toSeq.sortBy(-_.fee).take(limit)

  /**
   *
   * @param condition
   * @return
   */
  override def filter(condition: TX => Boolean): MemPool[TX] = {
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
}


object MemPool[TX] {
  lazy val emptyPool: MemPool[TX] = MemPool[TX](TrieMap())
}