package bifrost.state

import java.io.File
import java.util.UUID

import bifrost.crypto.FastCryptographicHash
import bifrost.modifier.ModifierId
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.modifier.box.{ Box, ProgramBox }
import bifrost.modifier.transaction.bifrostTransaction.Transaction
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }

import scala.util.Try

case class ProgramBoxRegistry(storage: LSMStore) extends Registry[UUID, ModifierId] {

  //----- input and output transformation functions
  // registry key input type -> ByteArrayWrapper (LSMStore input type)
  def registryInput ( key: UUID ): ByteArrayWrapper = {
    ByteArrayWrapper(
      FastCryptographicHash(
        BigInt(key.getMostSignificantBits).toByteArray ++ BigInt(key.getLeastSignificantBits).toByteArray))
  }


  // Array[Byte] (LSMStore store output type) -> registry key output type
  def registryOutput(value: Array[Byte]): ModifierId = ModifierId(value)

  // utxo store input type -> ByteArrayWrapper (LSMStore input type)
  def utxoInput(value: ModifierId): ByteArrayWrapper = ByteArrayWrapper(value.hashBytes)


  //YT NOTE - Using this function signature means boxes being removed from state must contain UUID (key) information
  //YT NOTE - Might be better to use transactions as parameters instead of boxes

  def updateFromState( newVersion: VersionTag,
                       keyFilteredBoxIdsToRemove: Set[Array[Byte]],
                       keyFilteredBoxesToAdd: Set[Box]): Try[ProgramBoxRegistry] =
    Try {
      log.debug(s"${Console.GREEN} Update ProgramBoxRegistry to version: ${newVersion.toString}${Console.RESET}")

      val boxIdsToRemove: Set[ByteArrayWrapper] =
        (keyFilteredBoxIdsToRemove -- keyFilteredBoxesToAdd.map(_.id)).map(ByteArrayWrapper.apply)

      //Getting all uuids being updated
      val uuidsToAppend: Map[UUID, Array[Byte]] =
        keyFilteredBoxesToAdd.filter(_.isInstanceOf[ProgramBox]).map(_.asInstanceOf[ProgramBox])
          .map(box => box.value -> box.id).toMap

      //Getting set of all boxes whose uuids are not being updated and hence should be tombstoned in LSMStore
      val uuidsToRemove: Set[UUID] =
        boxIdsToRemove
          .map(boxId => closedBox(boxId.data))
          .filter(box => box.isInstanceOf[ProgramBox])
          .map(_.asInstanceOf[ProgramBox])
          .filterNot(box => uuidsToAppend.contains(box.value))
          .map(_.value)

      storage.update(
        ByteArrayWrapper(newVersion.hashBytes),
        uuidsToRemove.map(registryInput),
        uuidsToAppend.map(e => ProgramBoxRegistry.uuidToBaw(e._1) -> ByteArrayWrapper(e._2))
      )

      ProgramBoxRegistry(storage)
  }


  def rollbackTo(version: VersionTag, stateStore: LSMStore): Try[ProgramBoxRegistry] = Try {
    if (storage.lastVersionID.exists(_.data sameElements version.hashBytes)) {
      this
    } else {
      log.debug(s"Rolling back ProgramBoxRegistry to: ${version.toString}")
      storage.rollback(ByteArrayWrapper(version.hashBytes))
      ProgramBoxRegistry(storage)
    }
  }

}

object ProgramBoxRegistry extends Logging {

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

