package bifrost.history

import com.google.common.primitives.Bytes
import bifrost.blocks.BifrostBlock
import scorex.core.NodeViewModifier
import scorex.core.NodeViewModifier.{ModifierId, ModifierTypeId}
import scorex.core.consensus.SyncInfo
import scorex.core.network.message.SyncInfoMessageSpec
import scorex.core.serialization.Serializer

import scala.util.Try

case class BifrostSyncInfo(override val answer: Boolean, lastBlockId: ModifierId, score: BigInt) extends SyncInfo {

  override def startingPoints: Seq[(ModifierTypeId, ModifierId)] = Seq(BifrostBlock.ModifierTypeId -> lastBlockId)


  override type M = BifrostSyncInfo

  override def serializer: Serializer[BifrostSyncInfo] = BifrostSyncInfoSerializer
}

object BifrostSyncInfo {
}

object BifrostSyncInfoSerializer extends Serializer[BifrostSyncInfo] {

  override def toBytes(obj: BifrostSyncInfo): Array[Byte] =
    (if (obj.answer) 1: Byte else 0: Byte) +: (obj.lastBlockId ++ obj.score.toByteArray)

  def parseBytes(bytes: Array[Byte]): Try[BifrostSyncInfo] = Try {
    val answer = if (bytes.head == 1) true else if (bytes.head == 0) false else throw new Exception("wrong answer byte")
    val mid = bytes.tail.take(NodeViewModifier.ModifierIdSize)
    val scoreBytes = bytes.tail.drop(NodeViewModifier.ModifierIdSize)
    BifrostSyncInfo(answer, mid, BigInt(scoreBytes))
  }
}

object BifrostSyncInfoMessageSpec extends SyncInfoMessageSpec[BifrostSyncInfo](BifrostSyncInfoSerializer.parseBytes)