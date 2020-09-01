package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.history.GenericHistory.ProgressInfo
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.nodeView.NodeViewModifier.idToBytes
import bifrost.utils.{BifrostEncoding, Logging}
import io.iohk.iodb.ByteArrayWrapper

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
    }
    else {
      chainCache.getParentId(parentId, _) match {
        case Some(parent) ⇒
          chainCache.add(block.id, parentId, chainCache.getParentHeight(parent).get + 1)
        case None ⇒
          log.warn(s"Received orphan block")
          ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
      }
    }

    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
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
