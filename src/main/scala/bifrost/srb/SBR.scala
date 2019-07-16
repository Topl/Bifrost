package bifrost.srb

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

class SBR (sbrStore: LSMStore, stateStore: LSMStore) extends ScorexLogging {

  def closedBox(boxId: Array[Byte]): Option[BifrostBox] =
    stateStore.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BifrostBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def getBoxId(k: UUID) : Option[Array[Byte]] =
    sbrStore.get(SBR.uuidToBaw(k))
      .map(_.data)

  def getBox(k: UUID) : Option[BifrostBox] =
    getBoxId(k).flatMap(closedBox(_))

  //Using this function signature means boxes being removed from state must contain UUID (key) information
  //And that the SBR needs to associate state's LSMStore as well to access the box by boxId (see BFR)
  //Might be better to use transactions as parameters instead of changes (boxesToRemove and boxesToAppend)
  def updateFromState(newVersion: VersionTag, changes: GSC): Try[SBR] = Try {

    val boxIdsToRemove: Set[ByteArrayWrapper] = (changes.boxIdsToRemove -- changes.toAppend.map(_.id)).map(ByteArrayWrapper.apply)

    //Getting all uuids being updated
    val uuidsToAppend: Map[UUID, Array[Byte]] =
      changes.toAppend.filter(_.isInstanceOf[BifrostProgramBox]).map(_.asInstanceOf[BifrostProgramBox])
      .map(box => box.value -> box.id).toMap

    //Getting set of all boxes whose uuids are not being updated and hence should be tombstoned in LSMStore
    val uuidsToRemove: Set[UUID] =
      boxIdsToRemove
        .flatMap(boxId => closedBox(boxId.data))
        .filter(box => box.isInstanceOf[BifrostProgramBox])
        .map(_.asInstanceOf[BifrostProgramBox])
        .filterNot(box => uuidsToAppend.contains(box.value))
        .map(_.value)

    sbrStore.update(
      ByteArrayWrapper(newVersion),
      uuidsToRemove.map(SBR.uuidToBaw(_)),
      uuidsToAppend.map(e => SBR.uuidToBaw(e._1) -> ByteArrayWrapper(e._2))
    )

    SBR(sbrStore, stateStore)
  }

  def updateFromState(versionTag: VersionTag, txs: Seq[BifrostTransaction]): Try[SBR] = Try {
    //TODO implement if boxes dont have UUIDs in them
    SBR(sbrStore, stateStore)
  }


  def rollbackTo(version: VersionTag, stateStore: LSMStore): Try[SBR] = Try {
    if (sbrStore.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rolling back SBR to: ${Base58.encode(version)}")
      sbrStore.rollback(ByteArrayWrapper(version))
      SBR(sbrStore, stateStore)
    }
  }

}

object SBR extends ScorexLogging {

  final val bytesInAUUID = 16
  final val bytesInABoxID = 32

  def apply(s1: LSMStore, s2:LSMStore) : SBR = {
    new SBR(s1, s2)
  }

  // UUID -> ByteArrayWrapper
  def uuidToBaw(v: UUID): ByteArrayWrapper = {
    ByteArrayWrapper(
      FastCryptographicHash(
        ByteArrayWrapper.fromLong(v.getMostSignificantBits).data ++
        ByteArrayWrapper.fromLong(v.getLeastSignificantBits).data))
  }

  def readOrGenerate(settings: ForgingSettings, stateStore: LSMStore): Option[SBR] = {
    val sbrDirOpt = settings.sbrDirOpt//.ensuring(_.isDefined, "sbr dir must be specified")
    val logDirOpt = settings.logDirOpt
    sbrDirOpt.map(readOrGenerate(_, logDirOpt, settings, stateStore))
  }

  def readOrGenerate(sbrDir: String, logDirOpt: Option[String], settings: ForgingSettings, stateStore: LSMStore): SBR = {
    val iFile = new File(s"$sbrDir")
    iFile.mkdirs()
    val sbrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing sbr storage...")
        sbrStore.close()
      }
    })

    SBR(sbrStore, stateStore)
  }

}

