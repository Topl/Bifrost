package examples.bifrost.mempool

import examples.bifrost.transaction.BifrostTransaction
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.transaction.MemoryPool
import scorex.core.utils.ScorexLogging

import scala.collection.concurrent.TrieMap
import scala.util.{Success, Try}


case class BifrostMemPool(unconfirmed: TrieMap[ByteArrayWrapper, BifrostTransaction])
  extends MemoryPool[BifrostTransaction, BifrostMemPool] with ScorexLogging {
  override type NVCT = BifrostMemPool

  private def key(id: Array[Byte]): ByteArrayWrapper = ByteArrayWrapper(id)

  //getters
  override def getById(id: ModifierId): Option[BifrostTransaction] =
  unconfirmed.get(key(id))

  override def contains(id: ModifierId): Boolean = unconfirmed.contains(key(id))

  override def getAll(ids: Seq[ModifierId]): Seq[BifrostTransaction] = ids.flatMap(getById)

  //modifiers
  override def put(tx: BifrostTransaction): Try[BifrostMemPool] = Success {
    unconfirmed.put(key(tx.id), tx)
    this
  }

  //todo
  override def put(txs: Iterable[BifrostTransaction]): Try[BifrostMemPool] = Success(putWithoutCheck(txs))

  override def putWithoutCheck(txs: Iterable[BifrostTransaction]): BifrostMemPool = {
    txs.foreach(tx => unconfirmed.put(key(tx.id), tx))
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
      condition(v)
    }
    this
  }

  override def size: Int = unconfirmed.size
}


object BifrostMemPool {
  lazy val emptyPool: BifrostMemPool = BifrostMemPool(TrieMap())
}