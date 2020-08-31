package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.history.GenericHistory.ProgressInfo
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.nodeView.NodeViewModifier.idToBytes
import bifrost.utils.BifrostEncoding
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.TreeMap

class BlockProcessor extends BifrostEncoding {

  /**
    * Process a single block and determine if any of the possible chains in the
    * chain cache are taller than the main chain section
    *
    * @param block - new block to put in cache
    * @return
    */
  def process(block: Block): ProgressInfo[Block] = {
    ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
  }
}

  object BlockProcessor {

    case class CacheBlock(id: ModifierId, height: Int)

  /**
    * Stores links mapping ((id, height) -> parentId) of blocks that could possibly be applied.
    */
  case class ChainCache(cache: TreeMap[CacheBlock, ModifierId]) {

    val nonEmpty: Boolean = cache.nonEmpty

    def getParentId(id: ModifierId, height: Int): Option[ModifierId] = cache.get(CacheBlock(id, height))

    def add(id: ModifierId, parentId: ModifierId, height: Int): ChainCache =
      ChainCache(cache.insert(CacheBlock(id, height), parentId))

    def dropUntil(height: Int): ChainCache =
      ChainCache(cache.dropWhile(_._1.height < height))
  }

  val BestChainMarker: Array[Byte] = Array(1: Byte)
  val NonBestChainMarker: Array[Byte] = Array(0: Byte)

  private implicit val ord: Ordering[CacheBlock] = Ordering[(Int, ModifierId)].on(x => (x.height, x.id))

  def emptyCache: ChainCache = ChainCache(TreeMap.empty)

  def chainStatusKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(FastCryptographicHash("main_chain".getBytes ++ idToBytes(id)))
}
