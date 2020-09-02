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
    val parentId = block.parentId
    if(history.contains(parentId)) {
      chainCache = chainCache.add (block, parentId, history.storage.heightOf(parentId).get + 1)
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    } else {
      chainCache.getParent(parentId) match {
        case Some(parent) ⇒
          val newHeight = parent.height + 1
          chainCache.add(block, parent.block.id, newHeight)
          if(newHeight > history.height) {
            val newChain = possibleChain(CacheBlock(block, newHeight))
            val commonAncestor = history.modifierById(newChain.head.parentId).get
            val oldChain = history.lastBlocks((history.height - history.storage.heightOf(commonAncestor.id).get), history.bestBlock)
            ProgressInfo(Some(commonAncestor.id), oldChain, newChain, Seq.empty)
          } else {
            ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
          }
        case None ⇒
          log.warn(s"Received orphan block")
          ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
      }
    }
  }

  private def possibleChain(from: CacheBlock): Seq[Block] = {
    @tailrec
    def loop(currBlock: Option[CacheBlock], height: Long, acc: Seq[Block]): Seq[Block] = {
      currBlock match {
        case Some(block) ⇒
          loop(chainCache.getParent(block.block.parentId), height - 1, block.block +: acc)
        case None ⇒ acc
      }
    }
    loop(Some(from), from.height, Seq.empty)
  }
}

  object BlockProcessor {

    case class CacheBlock(block: Block, height: Long)

  /**
    * Stores links mapping ((id, height) -> parentId) of blocks that could possibly be applied.
    */
  case class ChainCache(cache: TreeMap[CacheBlock, ModifierId]) {

    val nonEmpty: Boolean = cache.nonEmpty

    def getParentId(block: Block, height: Long): Option[ModifierId] = cache.get(CacheBlock(block, height))

    def getParent(id: ModifierId): Option[CacheBlock] = cache.keys.find(k ⇒ k.block.id == id)

    def getParentHeight(id: ModifierId): Option[Long] = cache.keys.find(k ⇒ k.block.id == id).map(_.height)

    def add(block: Block, parentId: ModifierId, height: Long): ChainCache =
      ChainCache(cache.insert(CacheBlock(block, height), parentId))

    def dropUntil(height: Long): ChainCache =
      ChainCache(cache.dropWhile(_._1.height < height))
  }

  val BestChainMarker: Array[Byte] = Array(1: Byte)
  val NonBestChainMarker: Array[Byte] = Array(0: Byte)

  private implicit val ord: Ordering[CacheBlock] = Ordering[(Long, ModifierId)].on(x => (x.height, x.block.id))

  def emptyCache: ChainCache = ChainCache(TreeMap.empty)

  def chainStatusKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(FastCryptographicHash("main_chain".getBytes ++ idToBytes(id)))
}
