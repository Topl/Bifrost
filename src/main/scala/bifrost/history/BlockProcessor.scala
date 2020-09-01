package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.history.GenericHistory.ProgressInfo
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.nodeView.NodeViewModifier.idToBytes
import bifrost.utils.{BifrostEncoding, Logging}
import io.iohk.iodb.ByteArrayWrapper

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

class BlockProcessor extends BifrostEncoding with Logging {

  import BlockProcessor._

  private var chainCache = emptyCache

  /**
    * Process a single block and determine if any of the possible chains in the
    * chain cache are taller than the main chain section
    *
    * @param block - new block to put in cache
    * @return
    */
  def process(history: History, block: Block): ProgressInfo[Block] = {
    val parentId = block.id
    if(history.contains(parentId)) {
      chainCache = chainCache.add (block.id, parentId, history.storage.heightOf(parentId).get + 1)
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    } else {
      chainCache.getParent(parentId) match {
        case Some(parent) ⇒
          chainCache.add(block.id, parent.id, parent.height + 1)
          ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
        case None ⇒
          log.warn(s"Received orphan block")
          ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
      }
    }
  }

  private def possibleChain(from: CacheBlock): Seq[ModifierId] = {
    @tailrec
    def loop(currBlock: Option[ModifierId], height: Long, acc: Seq[ModifierId]): Seq[ModifierId] = {
      currBlock match {
        case Some(block) ⇒
          loop(chainCache.getParentId(block, height), height - 1, block +: acc)
        case None ⇒ acc
      }
    }
    loop(Some(from.id), from.height, Seq.empty)
  }
}

  object BlockProcessor {

    case class CacheBlock(id: ModifierId, height: Long)

  /**
    * Stores links mapping ((id, height) -> parentId) of blocks that could possibly be applied.
    */
  case class ChainCache(cache: TreeMap[CacheBlock, ModifierId]) {

    val nonEmpty: Boolean = cache.nonEmpty

    def getParentId(id: ModifierId, height: Long): Option[ModifierId] = cache.get(CacheBlock(id, height))

    def getParent(id: ModifierId): Option[CacheBlock] = cache.keys.find(k ⇒ k.id == id)

    def getParentHeight(id: ModifierId): Option[Long] = cache.keys.find(k ⇒ k.id == id).map(_.height)

    def add(id: ModifierId, parentId: ModifierId, height: Long): ChainCache =
      ChainCache(cache.insert(CacheBlock(id, height), parentId))

    def dropUntil(height: Long): ChainCache =
      ChainCache(cache.dropWhile(_._1.height < height))
  }

  val BestChainMarker: Array[Byte] = Array(1: Byte)
  val NonBestChainMarker: Array[Byte] = Array(0: Byte)

  private implicit val ord: Ordering[CacheBlock] = Ordering[(Long, ModifierId)].on(x => (x.height, x.id))

  def emptyCache: ChainCache = ChainCache(TreeMap.empty)

  def chainStatusKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(FastCryptographicHash("main_chain".getBytes ++ idToBytes(id)))
}
