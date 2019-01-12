package bifrost.srb

import java.io.File
import java.util.UUID

import bifrost.forging.ForgingSettings
import bifrost.transaction.{BifrostTransaction, ContractCreation}
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.NodeViewModifier.ModifierId
import scorex.core.utils.ScorexLogging

import scala.util.Try

class StateBoxRegistry (initialMap: Map[ByteArrayWrapper, ByteArrayWrapper], storage: SBRStorage) extends ScorexLogging {

  private var UUID2BoxID = initialMap

  def updateIfStateBoxTransaction(tx: BifrostTransaction) : Unit = {

  }

  def update(k: UUID, v: Array[Byte]) : Unit = {
    val k_baw = StateBoxRegistry.uuid2baw(k)
    val v_baw = ByteArrayWrapper(v)
    val version = StateBoxRegistry.uuid2baw(UUID.randomUUID())
    storage.update(version, Seq((k_baw, v_baw)))
    UUID2BoxID += (k_baw -> v_baw)
  }

  def get(k: UUID) : Try[(UUID, Array[Byte])] = {
    val k_baw = StateBoxRegistry.uuid2baw(k)
    StateBoxRegistry.parseLine(Option(UUID2BoxID.getOrElse(k_baw, storage.get(k_baw).get)))
  }

  def checkpoint(modifierId: ModifierId): Try[Unit] = Try { storage.checkpoint(ByteArrayWrapper(modifierId)) }

  def rollback(modifierId: ModifierId): Try[Unit] = Try { storage.rollback(ByteArrayWrapper(modifierId)) }

}

object StateBoxRegistry extends ScorexLogging {

  final val bytesInAUUID = 16
  final val bytesInABoxID = 32

  def apply(s: SBRStorage) : Try[StateBoxRegistry] = Try {
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

  def readOrGenerate(settings: ForgingSettings): StateBoxRegistry = {
    val dataDirOpt = settings.sbrDirOpt.ensuring(_.isDefined, "sbr dir must be specified")
    val dataDir = dataDirOpt.get
    val logDirOpt = settings.logDirOpt
    readOrGenerate(dataDir, logDirOpt, settings)
  }

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: ForgingSettings): StateBoxRegistry = {
    val iFile = new File(s"$dataDir/map")
    iFile.mkdirs()
    val sbrStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing sbr storage...")
        sbrStorage.close()
      }
    })

    val storage = new SBRStorage(sbrStorage)

    StateBoxRegistry(storage).get
  }

}
