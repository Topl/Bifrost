package bifrost.history

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.ModifierId
import bifrost.nodeView.NodeViewModifier.idToBytes
import io.iohk.iodb.ByteArrayWrapper

import scala.collection.immutable.TreeMap

class BlockProcessor {




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

  private implicit val ord: Ordering[CacheBlock] = Ordering[(Int, Array[Byte])].on(x => (x.height, x.id.hashBytes))

  def emptyCache: ChainCache = ChainCache(TreeMap.empty)

  def chainStatusKey(id: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(FastCryptographicHash("main_chain".getBytes ++ idToBytes(id)))
}
