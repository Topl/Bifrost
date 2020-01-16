package bifrost.programBoxRegistry

import java.io.File
import java.util.UUID

import bifrost.NodeViewModifier.ModifierId
import bifrost.crypto.hash.FastCryptographicHash
import bifrost.forging.ForgingSettings
import bifrost.scorexMod.GenericMinimalState.VersionTag
import bifrost.state.BifrostState.GSC
import bifrost.transaction.bifrostTransaction.BifrostTransaction
import bifrost.transaction.box.{BifrostBox, BifrostBoxSerializer, BifrostProgramBox}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import bifrost.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.Try

case class ProgramBoxRegistry(pbrStore: LSMStore, stateStore: LSMStore) extends ScorexLogging {

  def closedBox(boxId: Array[Byte]): Option[BifrostBox] =
    stateStore.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BifrostBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def getBoxId(k: UUID) : Option[Array[Byte]] =
    pbrStore.get(ProgramBoxRegistry.uuidToBaw(k))
      .map(_.data)

  def getBox(k: UUID) : Option[BifrostBox] =
    getBoxId(k).flatMap(closedBox(_))


  //YT NOTE - Using this function signature means boxes being removed from state must contain UUID (key) information
  //YT NOTE - Might be better to use transactions as parameters instead of boxes

  def updateFromState(newVersion: VersionTag, keyFilteredBoxIdsToRemove: Set[Array[Byte]], keyFilteredBoxesToAdd: Set[BifrostBox]): Try[ProgramBoxRegistry] = Try {
    log.debug(s"${Console.GREEN} Update ProgramBoxRegistry to version: ${Base58.encode(newVersion)}${Console.RESET}")

    val boxIdsToRemove: Set[ByteArrayWrapper] = (keyFilteredBoxIdsToRemove -- keyFilteredBoxesToAdd.map(_.id)).map(ByteArrayWrapper.apply)

    //Getting all uuids being updated
    val uuidsToAppend: Map[UUID, Array[Byte]] =
      keyFilteredBoxesToAdd.filter(_.isInstanceOf[BifrostProgramBox]).map(_.asInstanceOf[BifrostProgramBox])
        .map(box => box.value -> box.id).toMap

    //Getting set of all boxes whose uuids are not being updated and hence should be tombstoned in LSMStore
    val uuidsToRemove: Set[UUID] =
      boxIdsToRemove
        .flatMap(boxId => closedBox(boxId.data))
        .filter(box => box.isInstanceOf[BifrostProgramBox])
        .map(_.asInstanceOf[BifrostProgramBox])
        .filterNot(box => uuidsToAppend.contains(box.value))
        .map(_.value)

    pbrStore.update(
      ByteArrayWrapper(newVersion),
      uuidsToRemove.map(ProgramBoxRegistry.uuidToBaw(_)),
      uuidsToAppend.map(e => ProgramBoxRegistry.uuidToBaw(e._1) -> ByteArrayWrapper(e._2))
    )

    ProgramBoxRegistry(pbrStore, stateStore)
  }

  //YT NOTE - implement if boxes dont have UUIDs in them
  def updateFromState(versionTag: VersionTag, txs: Seq[BifrostTransaction]): Try[ProgramBoxRegistry] = Try {
    ProgramBoxRegistry(pbrStore, stateStore)
  }


  def rollbackTo(version: VersionTag, stateStore: LSMStore): Try[ProgramBoxRegistry] = Try {
    if (pbrStore.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rolling back ProgramBoxRegistry to: ${Base58.encode(version)}")
      pbrStore.rollback(ByteArrayWrapper(version))
      ProgramBoxRegistry(pbrStore, stateStore)
    }
  }

}

object ProgramBoxRegistry extends ScorexLogging {

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

  def readOrGenerate(settings: ForgingSettings, stateStore: LSMStore): Option[ProgramBoxRegistry] = {
    val pbrDirOpt = settings.pbrDirOpt
    val logDirOpt = settings.logDirOpt
    pbrDirOpt.map(readOrGenerate(_, logDirOpt, settings, stateStore))
  }

  def readOrGenerate(pbrDir: String, logDirOpt: Option[String], settings: ForgingSettings, stateStore: LSMStore): ProgramBoxRegistry = {
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

