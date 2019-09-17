package bifrost.programBoxRegistry

import java.io.File
import java.util.UUID

import bifrost.NodeViewModifier.ModifierId
import bifrost.forging.ForgingSettings
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.StateBox
import com.google.common.primitives.Longs
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.utils.ScorexLogging
import scorex.crypto.hash.Sha256

import scala.util.Try
import scala.util.{Failure, Success}

//TODO remove
class ProgramBoxRegistryOld(initialMap: Map[ByteArrayWrapper, ByteArrayWrapper], storage: PBRStorage) extends ScorexLogging {

  var UUID2BoxID = initialMap

  def updateIfStateBoxTransaction(tx: BifrostTransaction) : Unit = {
//    tx.newBoxes.foreach(b => if b.isInstanceOf[StateBox])
  }


  def insertNewStateBox(modifierId: ModifierId, v: Array[Byte]): Try[(UUID, Array[Byte])] = Try {
    val k_uuid = UUID.nameUUIDFromBytes(v)
    update(modifierId, k_uuid, v)
    k_uuid -> v
  }

  def update(modifierId: ModifierId, k: UUID, v: Array[Byte]) : Unit = {
    val k_baw = ProgramBoxRegistryOld.uuid2baw(k)
    val v_baw = ByteArrayWrapper(v)
    storage.update(ByteArrayWrapper(modifierId), Seq((k_baw, v_baw)))
      match {
        case Success(_) =>
        case Failure(e) => new Exception("Unable to insert in StateBox registry")
      }
    UUID2BoxID += (k_baw -> v_baw)
  }

  def get(k: UUID) : Try[(UUID, Array[Byte])] = Try {
    val k_baw = ProgramBoxRegistryOld.uuid2baw(k)
    k -> UUID2BoxID.getOrElse(k_baw, storage.get(k_baw).get).data

//    ProgramBoxRegistryOld.parseLine(Option(UUID2BoxID.getOrElse(k_baw, storage.get(k_baw).get)))
  }

  def checkpoint(modifierId: ModifierId): Try[Unit] = Try { storage.checkpoint(ByteArrayWrapper(modifierId)) }

  def rollback(modifierId: ModifierId): Try[Unit] = Try { storage.rollback(ByteArrayWrapper(modifierId)) }

}

object ProgramBoxRegistryOld extends ScorexLogging {

  final val bytesInAUUID = 16
  final val bytesInABoxID = 32

  def apply(s: PBRStorage) : Try[ProgramBoxRegistryOld] = Try {
    new ProgramBoxRegistryOld(Map[ByteArrayWrapper, ByteArrayWrapper](), s)
  }

  //parsing a byteArrayWrapper which has UUID in bytes concatenated to boxID in bytes?
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
  //Currently appending UUID to itself to reach 32 byte length requirement for keys in LSMStore
  def uuid2baw(v: UUID) : ByteArrayWrapper =
//    ByteArrayWrapper(ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data
//    ++ ByteArrayWrapper.fromLong(v.getMostSignificantBits).data ++ ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data
//    ++ ByteArrayWrapper.fromLong(v.getMostSignificantBits).data)

    ByteArrayWrapper(ByteArrayWrapper.fromLong(v.getMostSignificantBits).data
      ++ ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data ++ ByteArrayWrapper.fromLong(v.getMostSignificantBits).data
      ++ ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data)

  def readOrGenerate(settings: ForgingSettings): ProgramBoxRegistryOld = {
    val pbrDirOpt = settings.pbrDirOpt.ensuring(_.isDefined, "programBoxRegistry dir must be specified")
    val pbrDir = pbrDirOpt.get
    val logDirOpt = settings.logDirOpt
    readOrGenerate(pbrDir, logDirOpt, settings)
  }

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: ForgingSettings): ProgramBoxRegistryOld = {
    val iFile = new File(s"$dataDir/map")
    iFile.mkdirs()
    val pbrStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing programBoxRegistry storage...")
        pbrStorage.close()
      }
    })

    val storage = new PBRStorage(pbrStorage)

    ProgramBoxRegistryOld(storage).get
  }


}
