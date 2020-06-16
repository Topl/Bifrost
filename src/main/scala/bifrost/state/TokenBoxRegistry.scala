package bifrost.state

import java.io.File

import bifrost.modifier.box._
import bifrost.modifier.box.proposition.PublicKey25519Proposition
import bifrost.settings.AppSettings
import bifrost.state.MinimalState.VersionTag
import bifrost.utils.Logging
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}

import scala.util.Try

case class TokenBoxRegistry(tbrStore: LSMStore, stateStore: LSMStore) extends Logging {

  def closedBox(boxId: Array[Byte]): Option[Box] =
    stateStore.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BoxSerializer.parseBytes)
      .flatMap(_.toOption)

  def boxIdsByKey(publicKey: PublicKey25519Proposition): Seq[Array[Byte]] =
    boxIdsByKey(publicKey.pubKeyBytes)

  //Assumes that boxIds are fixed length equal to store's keySize (32 bytes)
  def boxIdsByKey(pubKeyBytes: Array[Byte]): Seq[Array[Byte]] =
    tbrStore
    .get(ByteArrayWrapper(pubKeyBytes))
    .map(_
      .data
      .grouped(stateStore.keySize)
      .toSeq)
    .getOrElse(Seq[Array[Byte]]())

    def boxesByKey(publicKey: PublicKey25519Proposition): Seq[Box] =
    boxesByKey(publicKey.pubKeyBytes)

  def boxesByKey(pubKeyBytes: Array[Byte]): Seq[Box] = {
    boxIdsByKey(pubKeyBytes)
      .map(id => closedBox(id))
      .filter {
        case box: Some[Box] => true
        case None => false
      }
      .map(_.get)
  }

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
  def updateFromState(newVersion: VersionTag, keyFilteredBoxIdsToRemove: Set[Array[Byte]], keyFilteredBoxesToAdd: Set[Box]): Try[TokenBoxRegistry] = Try {
    log.debug(s"${Console.GREEN} Update TokenBoxRegistry to version: ${newVersion.toString}${Console.RESET}")

    /* This seeks to avoid the scenario where there is remove and then update of the same keys */
    val boxIdsToRemove: Set[ByteArrayWrapper] = (keyFilteredBoxIdsToRemove -- keyFilteredBoxesToAdd.map(b => b.id)).map(ByteArrayWrapper.apply)

    var boxesToRemove: Map[Array[Byte], Array[Byte]] = Map()
    var boxesToAppend: Map[Array[Byte], Array[Byte]] = Map()
    //Getting set of public keys for boxes being removed and appended
    //Using ByteArrayWrapper for sets since equality method uses a deep compare unlike a set of byte arrays
    val keysSet: Set[ByteArrayWrapper] = {
      boxIdsToRemove
        .flatMap(boxId => closedBox(boxId.data))
        .foreach(box => box match {
          case box: NoncedBox =>
            boxesToRemove += (box.id -> box.proposition.pubKeyBytes)
          //For boxes that do not follow the BifrostPublicKey25519NoncedBox (are not token boxes) - do nothing
          case _ =>
        })

      keyFilteredBoxesToAdd
        .foreach({
          case box: NoncedBox =>
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

    tbrStore.update(
      ByteArrayWrapper(newVersion.hashBytes),
      Seq(),
      keysToBoxIds.map(element =>
        element._1 -> ByteArrayWrapper(element._2.flatten.toArray))
    )

    TokenBoxRegistry(tbrStore, stateStore)
  }

  def rollbackTo(version: VersionTag, stateStore: LSMStore): Try[TokenBoxRegistry] = Try {
    if (tbrStore.lastVersionID.exists(_.data sameElements version.hashBytes)) {
      this
    } else {
      log.debug(s"Rolling back TokenBoxRegistry to: ${version.toString}")
      tbrStore.rollback(ByteArrayWrapper(version.hashBytes))
      TokenBoxRegistry(tbrStore, stateStore)
    }
  }

}

object TokenBoxRegistry extends Logging {

  def apply(s1: LSMStore, s2: LSMStore) : TokenBoxRegistry = {
    new TokenBoxRegistry (s1, s2)
  }

  def readOrGenerate(settings: AppSettings, stateStore: LSMStore): Option[TokenBoxRegistry] = {
    val tbrDir = settings.tbrDir
    val logDir = settings.logDir
    tbrDir.map(readOrGenerate(_, logDir, settings, stateStore))
  }

  def readOrGenerate(tbrDir: String, logDirOpt: Option[String], settings: AppSettings, stateStore: LSMStore): TokenBoxRegistry = {
    val iFile = new File(s"$tbrDir")
    iFile.mkdirs()
    val tbrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing tokenBoxRegistry storage...")
        tbrStore.close()
      }
    })

    TokenBoxRegistry(tbrStore, stateStore)
  }

}
