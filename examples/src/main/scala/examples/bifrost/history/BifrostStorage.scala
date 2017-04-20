package examples.bifrost.history

import com.google.common.primitives.Longs
import examples.bifrost.blocks.{BifrostBlock, BifrostBlockCompanion}
import examples.bifrost.forging.{Forger, ForgingConstants}
import examples.hybrid.blocks._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.NodeViewModifier._
import scorex.core.utils.ScorexLogging
import scorex.crypto.hash.Sha256

import scala.util.Failure

class BifrostStorage(storage: LSMStore,
                     settings: ForgingConstants) extends ScorexLogging {

  private val bestBlockIdKey = ByteArrayWrapper(Array.fill(storage.keySize)(-1: Byte))

  def height: Long = heightOf(bestBlockId).getOrElse(0L)

  def bestChainScore: Long = height

  def bestBlockId: Array[Byte] = storage.get(bestBlockIdKey).map(_.data).getOrElse(settings.GenesisParentId)

  def bestBlock: BifrostBlock = {
    require(height > 0, "History is empty")
    modifierById(bestBlockId).get
  }

  def modifierById(blockId: ModifierId): Option[BifrostBlock] = {
    storage.get(ByteArrayWrapper(blockId)).flatMap { bw =>
      val bytes = bw.data
      val mtypeId = bytes.head // TODO could check that this is a bifrostblock
      val parsed = BifrostBlockCompanion.parseBytes(bytes.tail)
      parsed match {
        case Failure(e) => log.warn("Failed to parse bytes from bd", e)
        case _ =>
      }
      parsed.toOption
    }
  }

  def update(b: BifrostBlock, diff: Option[Long], isBest: Boolean) {
    log.debug(s"Write new best=$isBest block ${b.encodedId}")
    val typeByte = BifrostBlock.ModifierTypeId

    val blockH: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] =
      Seq(blockHeightKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(parentHeight(b) + 1)))

    val blockDiff: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = diff.map { d =>
      Seq(blockDiffKey(b.id) -> ByteArrayWrapper(Longs.toByteArray(d)))
    }.getOrElse(Seq())

    val bestBlock: Iterable[(ByteArrayWrapper, ByteArrayWrapper)] = Seq(bestBlockIdKey -> ByteArrayWrapper(b.id))

    storage.update(
      ByteArrayWrapper(b.id),
      Seq(),
      blockDiff ++ blockH ++ bestBlock ++ Seq(ByteArrayWrapper(b.id) -> ByteArrayWrapper(typeByte +: b.bytes)))
  }

  private def blockScoreKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("score".getBytes ++ blockId))

  private def blockHeightKey(blockId: ModifierId): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("height".getBytes ++ blockId))

  private def blockDiffKey(blockId: Array[Byte]): ByteArrayWrapper =
    ByteArrayWrapper(Sha256("difficulty".getBytes ++ blockId))

  def scoreOf(blockId: ModifierId): Option[Long] = storage.get(blockScoreKey(blockId)).map(b => Longs.fromByteArray(b.data))
  def heightOf(blockId: ModifierId): Option[Long] = storage.get(blockHeightKey(blockId)).map(b => Longs.fromByteArray(b.data))
  def difficultyOf(blockId: ModifierId): Option[Long] = if (blockId sameElements settings.GenesisParentId) {
    Some(Forger.InitialDifficuly)
  } else {
    storage.get(blockDiffKey(blockId)).map(b => Longs.fromByteArray(b.data))
  }

  def parentChainScore(b: BifrostBlock): Long = scoreOf(b.parentId).getOrElse(0L)
  def parentHeight(b: BifrostBlock): Long = heightOf(b.parentId).getOrElse(0L)
  def parentDifficulty(b: BifrostBlock): Long = difficultyOf(b.parentId).getOrElse(0L)

  def isGenesis(b: BifrostBlock): Boolean = b.parentId sameElements settings.GenesisParentId

}
