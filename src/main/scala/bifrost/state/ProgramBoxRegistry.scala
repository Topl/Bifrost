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

case class ProgramBoxRegistry(storage: LSMStore) extends Registry[ProgramBoxRegistry.K, ProgramBoxRegistry.V] {

  import ProgramBoxRegistry.{K, V}

  //----- input and output transformation functions
  override def registryInput (key: K): Array[Byte] = key.hashBytes
  override def registryOutput (value: Array[Byte]): V = value

  /**
   * @param newVersion - block id
   * @param keyFilteredBoxIdsToRemove
   * @param keyFilteredBoxesToAdd
   * - key filtered boxIdsToRemove and boxesToAppend extracted from block (in BifrostState)
   * @return - instance of updated TokenBoxRegistry
   * (Note - makes use of vars for local variables since function remains a pure function and helps achieve better runtime)
   *
   *         Runtime complexity of below function is O(MN) + O(L)
   *         where M = Number of boxes to remove
   *         N = Number of boxes owned by a public key
   *         L = Number of boxes to append
   *
   */
  //noinspection ScalaStyle
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
          .map(boxId => lookup(boxId.data))
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

  type K = ProgramId
  type V = Array[Byte]

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

    ProgramBoxRegistry(pbrStore)
  }

}

