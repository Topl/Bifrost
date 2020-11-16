package co.topl.nodeView.mempool

import co.topl.attestation.proof.Proof
import co.topl.attestation.proposition.Proposition
import co.topl.modifier.ModifierId
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.box.{ Box, BoxId }
import co.topl.utils.Logging

import scala.collection.concurrent.TrieMap
import scala.util.Try

case class MemPool(unconfirmed: TrieMap[ModifierId, Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]])
  extends MemoryPool[Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]], MemPool] with Logging {

  override type NVCT = MemPool
  type TX = Transaction[_, _ <: Proposition, _ <: Proof[_], _ <: Box[_]]

  private val boxesInMempool = new TrieMap[BoxId, BoxId]()

  //getters
  override def modifierById(id: ModifierId): Option[TX] = unconfirmed.get(id)

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(id)

  override def getAll(ids: Seq[ModifierId]): Seq[TX] = ids.flatMap(modifierById)

  override def size: Int = unconfirmed.size

  //modifiers
  override def put(tx: TX): Try[MemPool] = Try {
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
  override def put(txs: Iterable[TX]): Try[MemPool] = Try {
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
  override def putWithoutCheck(txs: Iterable[TX]): MemPool = {
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
  override def remove(tx: TX): MemPool = {
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
  override def filter(condition: TX => Boolean): MemPool = {
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


object MemPool {
  lazy val emptyPool: MemPool = MemPool(TrieMap())
}