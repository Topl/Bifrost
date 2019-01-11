package bifrost.srb

import java.util.UUID

import bifrost.history.SRBStorage
import com.google.common.primitives.Longs
import io.iohk.iodb.ByteArrayWrapper
import scorex.core.NodeViewModifier.ModifierId

import scala.util.Try

case class StateBoxRegistry (initialMap: Map[ByteArrayWrapper, ByteArrayWrapper], storage: SRBStorage)  {

  private val UUID2BoxID = initialMap

  def update(k: UUID, v: Array[Byte]) : Unit = {
    val k_baw = StateBoxRegistry.uuid2baw(k)
    val v_baw = ByteArrayWrapper(v)
    val version = StateBoxRegistry.uuid2baw(UUID.randomUUID())
    storage.update(version, Seq((k_baw, v_baw)))
    UUID2BoxID(k) = v
  }

  def get(k: UUID) : Try[(UUID, Array[Byte])] = {
    val k_baw = StateBoxRegistry.uuid2baw(k)
    StateBoxRegistry.parseLine(Option(UUID2BoxID.getOrElse(k_baw, storage.get(k_baw).get)))
  }

  def checkpoint(modifierId: ModifierId): Try[Unit] = Try { storage.checkpoint(ByteArrayWrapper(modifierId)) }

  def rollback(modifierId: ModifierId): Try[Unit] = Try { storage.rollback(ByteArrayWrapper(modifierId)) }

}

object StateBoxRegistry {

  final val bytesInAUUID = 16
  final val bytesInABoxID = 32

  def apply(s: SRBStorage) : Try[StateBoxRegistry] = Try {
    new StateBoxRegistry(Map[ByteArrayWrapper, ByteArrayWrapper](), s)
  }

  def parseLine(raw: Option[ByteArrayWrapper]) : Try[(UUID, Array[Byte])] = Try {
    val rawLine : Array[Byte] = raw.get.data
    val uUIDBytes = rawLine.take(bytesInAUUID)
    val iDBytes = rawLine.slice(bytesInAUUID, bytesInAUUID + bytesInABoxID)
    (
      new UUID(Longs.fromByteArray(uUIDBytes.take(Longs.BYTES)), Longs.fromByteArray(uUIDBytes.slice(Longs.BYTES, Longs.BYTES*2))),
      iDBytes
    )
  }

  // UUID -> ByteArrayWrapper
  def uuid2baw(v: UUID) : ByteArrayWrapper = ByteArrayWrapper(ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data
    ++ ByteArrayWrapper.fromLong(v.getMostSignificantBits).data)


}
