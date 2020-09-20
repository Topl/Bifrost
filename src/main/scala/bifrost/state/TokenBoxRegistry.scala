package bifrost.state

import java.io.File

import bifrost.modifier.ModifierId
import bifrost.modifier.box._
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.modifier.box.serialization.BoxSerializer
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ ByteArrayWrapper, LSMStore }

import scala.util.Try

case class TokenBoxRegistry(storage: LSMStore) extends Registry[PublicKey25519Proposition, ModifierId] {

  //----- input and output transformation functions
  // registry key input type -> ByteArrayWrapper (LSMStore input type)
  def registryInput(key: PublicKey25519Proposition): ByteArrayWrapper = ByteArrayWrapper(key.pubKeyBytes)

  // Array[Byte] (LSMStore store output type) -> registry key output type
  def registryOutput(value: Array[Byte]): ModifierId = ModifierId(value)

  // utxo store input type -> ByteArrayWrapper (LSMStore input type)
  def utxoInput(value: ModifierId): ByteArrayWrapper = ByteArrayWrapper(value.hashBytes)


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
  override def updateFromState(newVersion: VersionTag,
                               utxoStore: DB,
                               boxIdsToRemove: Set[ModifierId],
                               boxesToAdd: Set[Box]): Try[TokenBoxRegistry] = {
    Try {
      log.debug(s"${Console.GREEN} Update TokenBoxRegistry to version: ${newVersion.toString}${Console.RESET}")

      /* This seeks to avoid the scenario where there is remove and then update of the same keys */
      val toRemove: Set[ByteArrayWrapper] =
        (boxIdsToRemove.map(b => b.hashBytes) -- boxesToAdd.map(b => b.id)).map(ByteArrayWrapper.apply)

      var boxesToRemove: Map[Array[Byte], Array[Byte]] = Map()
      var boxesToAppend: Map[Array[Byte], Array[Byte]] = Map()

      //Getting set of public keys for boxes being removed and appended
      //Using ByteArrayWrapper for sets since equality method uses a deep compare unlike a set of byte arrays
      val keysSet: Set[ByteArrayWrapper] = {
        boxIdsToRemove
          .flatMap(boxId => closedBox(boxId.data))
          .foreach {
            case box: TokenBox =>
              boxesToRemove += (box.id -> box.proposition.pubKeyBytes)
            //For boxes that do not follow the BifrostPublicKey25519NoncedBox (are not token boxes) - do nothing
            case _ =>
          }

        boxesToAdd
          .foreach({
            case box: TokenBox =>
              boxesToAppend += (box.id -> box.proposition.pubKeyBytes)
            //For boxes that do not follow the BifrostPublicKey25519NoncedBox (are not token boxes) - do nothing
            case _ =>
          })

        (boxesToRemove.map(boxToKey => ByteArrayWrapper(boxToKey._2)) ++ boxesToAppend.map(boxToKey => ByteArrayWrapper(boxToKey._2))).toSet
      }

      //Get old boxIds list for each of the above public keys
      var keysToBoxIds: Map[ByteArrayWrapper, Seq[Array[Byte]]] = keysSet.map(
        publicKey => publicKey -> boxIdsByKey(publicKey.data)
      ).toMap

      //For each box in temporary map match against public key and remove/append to boxIdsList in original keysToBoxIds map
      for((boxId, publicKey) <- boxesToRemove) {
        keysToBoxIds += (ByteArrayWrapper(publicKey) -> keysToBoxIds(ByteArrayWrapper(publicKey)).filterNot(_ sameElements boxId))
      }
      for((boxId, publicKey) <- boxesToAppend) {
        //Prepending to list is O(1) while appending is O(n)
        keysToBoxIds += (ByteArrayWrapper(publicKey) -> (boxId +: keysToBoxIds(ByteArrayWrapper(publicKey))))
      }

      storage.update(
        ByteArrayWrapper(newVersion.hashBytes),
        Seq(),
        keysToBoxIds.map(element =>
          element._1 -> ByteArrayWrapper(element._2.flatten.toArray))
      )

      TokenBoxRegistry(storage)
  }
  }

  override def rollbackTo(version: VersionTag): Try[TokenBoxRegistry] = Try {
    if (storage.lastVersionID.exists(_.data sameElements version.hashBytes)) {
      this
    } else {
      log.debug(s"Rolling back TokenBoxRegistry to: ${version.toString}")
      storage.rollback(ByteArrayWrapper(version.hashBytes))
      TokenBoxRegistry(storage)
    }
  }

}

object TokenBoxRegistry extends Logging {

  def readOrGenerate(settings: AppSettings, stateStore: LSMStore): Option[TokenBoxRegistry] = {
    val tbrDir = settings.tbrDir
    val logDir = settings.logDir
    tbrDir.map(readOrGenerate(_, logDir, settings, stateStore))
  }

  def readOrGenerate(tbrDir: String,
                     logDirOpt: Option[String],
                     settings: AppSettings): TokenBoxRegistry = {

    val iFile = new File(s"$tbrDir")
    iFile.mkdirs()
    val tbrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing tokenBoxRegistry storage...")
        tbrStore.close()
      }
    })

    TokenBoxRegistry(tbrStore)
  }

}
