package bifrost.state

import java.io.File
import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.box.{Box, BoxSerializer, ProgramBox}
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

case class ProgramBoxRegistry(pbrStore: LSMStore, stateStore: LSMStore) extends Logging {

  def closedBox(boxId: Array[Byte]): Option[Box] =
    stateStore.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def getBoxId(k: UUID) : Option[Array[Byte]] =
    pbrStore.get(ProgramBoxRegistry.uuidToBaw(k))
      .map(_.data)

  def getBox(k: UUID) : Option[Box] =
    getBoxId(k).flatMap(closedBox(_))


  //YT NOTE - Using this function signature means boxes being removed from state must contain UUID (key) information
  //YT NOTE - Might be better to use transactions as parameters instead of boxes

  def updateFromState(newVersion: VersionTag, keyFilteredBoxIdsToRemove: Set[Array[Byte]], keyFilteredBoxesToAdd: Set[Box]): Try[ProgramBoxRegistry] = Try {
    log.debug(s"${Console.GREEN} Update ProgramBoxRegistry to version: ${newVersion.toString}${Console.RESET}")

    val boxIdsToRemove: Set[ByteArrayWrapper] = (keyFilteredBoxIdsToRemove -- keyFilteredBoxesToAdd.map(_.id)).map(ByteArrayWrapper.apply)

    //Getting all uuids being updated
    val uuidsToAppend: Map[UUID, Array[Byte]] =
      keyFilteredBoxesToAdd.filter(_.isInstanceOf[ProgramBox]).map(_.asInstanceOf[ProgramBox])
        .map(box => box.value -> box.id).toMap

    //Getting set of all boxes whose uuids are not being updated and hence should be tombstoned in LSMStore
    val uuidsToRemove: Set[UUID] =
      boxIdsToRemove
        .flatMap(boxId => closedBox(boxId.data))
        .filter(box => box.isInstanceOf[ProgramBox])
        .map(_.asInstanceOf[ProgramBox])
        .filterNot(box => uuidsToAppend.contains(box.value))
        .map(_.value)

    pbrStore.update(
      ByteArrayWrapper(newVersion.hashBytes),
      uuidsToRemove.map(ProgramBoxRegistry.uuidToBaw(_)),
      uuidsToAppend.map(e => ProgramBoxRegistry.uuidToBaw(e._1) -> ByteArrayWrapper(e._2))
    )

    ProgramBoxRegistry(pbrStore, stateStore)
  }

  //YT NOTE - implement if boxes dont have UUIDs in them
  def updateFromState(versionTag: VersionTag, txs: Seq[Transaction]): Try[ProgramBoxRegistry] = Try {
    ProgramBoxRegistry(pbrStore, stateStore)
  }


  def rollbackTo(version: VersionTag, stateStore: LSMStore): Try[ProgramBoxRegistry] = Try {
    if (pbrStore.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rolling back ProgramBoxRegistry to: ${version.toString}")
      pbrStore.rollback(ByteArrayWrapper(version.hashBytes))
      ProgramBoxRegistry(pbrStore, stateStore)
    }
  }

}

object ProgramBoxRegistry extends Logging {

  final val bytesInAUUID = 16
  final val bytesInABoxID = 32

  def apply(s1: LSMStore, s2:LSMStore) : ProgramBoxRegistry = {
    new ProgramBoxRegistry(s1, s2)
  }

  // UUID -> ByteArrayWrapper
  def uuidToBaw(v: UUID): ByteArrayWrapper = {
    ByteArrayWrapper(
      FastCryptographicHash(
        ByteArrayWrapper.fromLong(v.getMostSignificantBits).data ++
        ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data))
  }

  def readOrGenerate(settings: AppSettings, stateStore: LSMStore): Option[ProgramBoxRegistry] = {
    val pbrDirOpt = settings.pbrDir
    val logDirOpt = settings.logDir
    pbrDirOpt.map(readOrGenerate(_, logDirOpt, settings, stateStore))
  }

  def readOrGenerate(pbrDir: String, logDirOpt: Option[String], settings: AppSettings, stateStore: LSMStore): ProgramBoxRegistry = {
    val iFile = new File(s"$pbrDir")
    iFile.mkdirs()
    val pbrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing programBoxRegistry storage...")
        pbrStore.close()
      }
    })

    ProgramBoxRegistry(pbrStore, stateStore)
  }

}

