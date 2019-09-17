package bifrost.history

import bifrost.blocks.BifrostBlock
import com.google.common.primitives.Longs
import bifrost.NodeViewModifier
import bifrost.NodeViewModifier.{ModifierId, ModifierTypeId}
import bifrost.consensus.SyncInfo
import bifrost.network.message.SyncInfoMessageSpec
import bifrost.serialization.Serializer

import scala.util.Try

case class BifrostSyncInfo(override val answer: Boolean, lastBlockIds: Seq[ModifierId], score: BigInt)
  extends SyncInfo {

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] =
    lastBlockIds.map(b => BifrostBlock.ModifierTypeId -> b)

  override type M = BifrostSyncInfo

  override def serializer: Serializer[BifrostSyncInfo] = BifrostSyncInfoSerializer
}

object BifrostSyncInfo {
  val MaxLastBlocks = 50 //don't make it more than 127 without changing serialization!
}

object BifrostSyncInfoSerializer extends Serializer[BifrostSyncInfo] {

  override def toBytes(obj: BifrostSyncInfo): Array[Byte] =
    Array(if (obj.answer) 1: Byte else 0: Byte,
          obj.lastBlockIds.size.toByte
    ) ++ obj.lastBlockIds.foldLeft(Array[Byte]())((a, b) => a ++ b) ++
      Longs.toByteArray(obj.score.toByteArray.length) ++ obj.score.toByteArray

  def parseBytes(bytes: Array[Byte]): Try[BifrostSyncInfo] = Try {
    val answer = if (bytes.head == 1) true else if (bytes.head == 0) false else throw new Exception("wrong answer byte")
    val lastBlockIdsSize = bytes.slice(1, 2).head
    val endOfBlockIds = 2 + lastBlockIdsSize * NodeViewModifier.ModifierIdSize
    val scoreByteSize = Longs.fromByteArray(bytes.slice(endOfBlockIds, endOfBlockIds + Longs.BYTES))

    require(lastBlockIdsSize >= 0 && lastBlockIdsSize <= BifrostSyncInfo.MaxLastBlocks)
    require(bytes.length == 2 + lastBlockIdsSize * NodeViewModifier.ModifierIdSize + Longs.BYTES + scoreByteSize)

    val lastBlockIds = bytes
      .slice(2, endOfBlockIds)
      .grouped(NodeViewModifier.ModifierIdSize).toSeq

    val scoreBytes = bytes.slice(endOfBlockIds + Longs.BYTES, bytes.length)
    BifrostSyncInfo(answer, lastBlockIds, BigInt(scoreBytes))
  }
}

object BifrostSyncInfoMessageSpec extends SyncInfoMessageSpec[BifrostSyncInfo](BifrostSyncInfoSerializer.parseBytes)