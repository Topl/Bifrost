package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.history.BlockProcessor.ChainCache
import bifrost.history.GenericHistory.ProgressInfo
import bifrost.modifier.ModifierId
import bifrost.modifier.block.Block
import bifrost.nodeView.NodeViewModifier.idToBytes
import bifrost.utils.{BifrostEncoding, Logging}
import io.iohk.iodb.ByteArrayWrapper

import scala.annotation.tailrec
import scala.collection.immutable.TreeMap

class BlockProcessor(cache: ChainCache) extends BifrostEncoding with Logging {

  import BlockProcessor._

  private var chainCache = cache

  /**
   * Publicly accessible method to check if a block can be applied into the cache
   *
   * @param modifier block that we are attempting to apply into the cache
   * @return 'true' if the parent ID of the given modifier is available in the cache
   */
  def applicableInCache(modifier: Block): Boolean = chainCache.getCacheBlock(modifier.parentId).nonEmpty

  /**
    * Process a single block and determine if any of the possible chains in the
    * chain cache are taller than the main chain section
    *
    * @param block - new block to put in cache
    * @return
    */
  def process(history: History, block: Block): ProgressInfo[Block] = {
    // check if the current block is starting a new branch off the main chain
    if (history.applicable(block)) {
      chainCache = chainCache.add(block, history.storage.heightOf(block.parentId).get + 1)
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)

    // if not starting a new branch, can it extend an already known tine?
    } else if (applicableInCache(block)) {
        val newHeight = chainCache.getHeight(block.parentId).get + 1
        chainCache = chainCache.add(block, newHeight)

        // if new chain is longer, calculate and return the ProgressInfo needed to switch tines
        if (newHeight > history.height) {
          val newChain = possibleChain(CacheBlock(block, newHeight))
          val commonAncestor = history.modifierById(newChain.head.parentId).get
          val oldChain = history.lastBlocks(
            history.height - history.storage.heightOf(commonAncestor.id).get,
            history.bestBlock )

          ProgressInfo(Some(commonAncestor.id), oldChain, newChain, Seq.empty)

        // otherwise, exit after adding the new block to the chain cache
        } else ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)

    // if no parent, log and do not add the block to the chain cache
    } else {
      log.warn(s"Received orphan block")
      ProgressInfo(None, Seq.empty, Seq.empty, Seq.empty)
    }
  }

  /**
   * construct a sequence of blocks to be switched to
   *
   * @param from starting point to recurse from at the tip of the chain
   * @return a sequence of blocks from the new highest block to the common ancestor
   */
  private def possibleChain(from: CacheBlock): Seq[Block] = {
    @tailrec
    def loop( currBlock: Option[CacheBlock], height: Long, acc: Seq[Block]): Seq[Block] = {
      currBlock match {
        case Some(b) ⇒ loop(chainCache.getCacheBlock(b.block.parentId), height - 1, b.block +: acc)
        case None ⇒ acc
      }
    }

    loop(Some(from), from.height, Seq.empty)
  }
}

object BlockProcessor {

  def apply(): BlockProcessor = new BlockProcessor(emptyCache)

  def emptyCache: ChainCache = ChainCache(TreeMap.empty)

  def chainStatusKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(
      FastCryptographicHash("main_chain".getBytes ++ idToBytes(id))
    )

  private implicit val ord: Ordering[CacheBlock] =
    Ordering[(Long, ModifierId)].on(x => (x.height, x.block.id))

  /** Wrapper for storing a block and its height in the chain cache */
  case class CacheBlock(block: Block, height: Long)

  /** Stores links mapping ((id, height) -> parentId) of blocks that could possibly be applied. */
  case class ChainCache(cache: TreeMap[CacheBlock, ModifierId]) {

    val nonEmpty: Boolean = cache.nonEmpty

    def getParentId(block: Block, height: Long): Option[ModifierId] =
      cache.get(CacheBlock(block, height))

    def getCacheBlock(id: ModifierId): Option[CacheBlock] =
      cache.keys.find(k ⇒ k.block.id == id)

    def getHeight(id: ModifierId): Option[Long] =
      cache.keys.find(k ⇒ k.block.id == id).map(_.height)

    def add(block: Block, height: Long): ChainCache =
      ChainCache(cache.insert(CacheBlock(block, height), block.parentId))

    def dropUntil(height: Long): ChainCache =
      ChainCache(cache.dropWhile(_._1.height < height))

    // todo: JAA - should have some way to clean up the cache so it doesn't keep growing
    //             Perhaps dropping when their height is below a max value? This could involve
    //             tracking the max height and comparing as blocks are placed in.
  }
}
