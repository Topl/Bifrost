package bifrost.bfr

import java.io.File

import bifrost.forging.ForgingSettings
import bifrost.scorexMod.GenericMinimalState.VersionTag
import bifrost.state.BifrostState.{BX, GSC}
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.utils.ScorexLogging
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.encode.Base58

import scala.util.Try

class BFR(bfrStore: LSMStore, stateStore: LSMStore) extends ScorexLogging {

//  def closedBox(boxId: Array[Byte]): Option[BX] =
//    stateStore.get(ByteArrayWrapper(boxId))
//      .map(_.data)
//      .map(BifrostBoxSerializer.parseBytes)
//      .flatMap(_.toOption)

    def closedBox(boxId: Array[Byte]): Option[BifrostPublic25519NoncedBox] =
      stateStore.get(ByteArrayWrapper(boxId))
        .map(_.data)
        .map(BifrostBoxSerializer.parseBytes)
        .flatMap(_.toOption) match {
        case Some(p) => Some(p.asInstanceOf[BifrostPublic25519NoncedBox])
        case None => None
      }


  def boxIdsByKey(publicKey: PublicKey25519Proposition): Seq[Array[Byte]] =
    boxIdsByKey(publicKey.pubKeyBytes)

  //Assumes that boxIds are fixed length equal to store's keySize (32 bytes)
  def boxIdsByKey(pubKeyBytes: Array[Byte]): Seq[Array[Byte]] = bfrStore
    .get(ByteArrayWrapper(pubKeyBytes))
    .map(_
      .data
      .grouped(stateStore.keySize)
      .toSeq)
    .getOrElse(Seq[Array[Byte]]())

//  def boxesByKey(publicKey: PublicKey25519Proposition): Seq[BifrostBox] =
//    boxesByKey(publicKey.pubKeyBytes)
//
//  def boxesByKey(pubKeyBytes: Array[Byte]): Seq[BifrostBox] = {
//    boxIdsByKey(pubKeyBytes)
//      .map(id => closedBox(id))
//      .filter {
//        case box: Some[BifrostBox] => true
//        case None => false
//      }
//      .map(_.get)
//  }

  def boxesByKey(publicKey: PublicKey25519Proposition): Seq[BifrostPublic25519NoncedBox] =
    boxesByKey(publicKey.pubKeyBytes)

  def boxesByKey(pubKeyBytes: Array[Byte]): Seq[BifrostPublic25519NoncedBox] = {
    boxIdsByKey(pubKeyBytes)
      .map(id => closedBox(id))
      .filter {
        case box: Some[BifrostPublic25519NoncedBox] => true
        case None => false
      }
      .map(_.get)
  }


//noinspection ScalaStyle
def updateFromState(newVersion: VersionTag, changes: GSC): BFR = {
  log.debug(s"${Console.GREEN} Update BFR to version: ${Base58.encode(newVersion)}${Console.RESET}")

//  println()
//  println("Boxes to remove")
//  changes.boxIdsToRemove.flatMap(
//    id => closedBox(id))
//    .foreach(box => println(box.json)
//    )
//  println()
//  println("Boxes to append")
//  changes.toAppend.foreach(box => println(box.json))

  var boxesToRemove: Map[Array[Byte], Array[Byte]] = Map()
  var boxesToAppend: Map[Array[Byte], Array[Byte]] = Map()
  //Getting set of public keys for boxes being removed and appended
  //Using ByteArrayWrapper for sets since equality method uses a deep compare unlike a set of byte arrays
  val keysSet: Set[ByteArrayWrapper] = {
    changes.boxIdsToRemove
      .flatMap(boxId => closedBox(boxId))
      .foreach(box => box match {
        //Filtering token boxes (these are the only boxes whose propositions are assured to be publicKeys)
        //Adding other boxes to BFR needs box re-architecture so that proposition format matches those of token boxes
        case arbit: ArbitBox => {
          boxesToRemove += (arbit.id -> arbit.proposition.pubKeyBytes)
        }
        case asset: AssetBox => {
          boxesToRemove += (asset.id -> asset.proposition.pubKeyBytes)
        }
        case poly: PolyBox => {
          boxesToRemove += (poly.id -> poly.proposition.pubKeyBytes)
        }
        case _ =>
      })

    changes.toAppend
      .foreach({
        case arbit: ArbitBox => {
          boxesToAppend += (arbit.id -> arbit.proposition.pubKeyBytes)
        }
        case asset: AssetBox => {
          boxesToAppend += (asset.id -> asset.proposition.pubKeyBytes)
        }
        case poly: PolyBox => {
          boxesToAppend += (poly.id -> poly.proposition.pubKeyBytes)
        }
        case _ =>
      })

    (boxesToRemove.map(boxToKey => ByteArrayWrapper(boxToKey._2)) ++ boxesToAppend.map(boxToKey => ByteArrayWrapper(boxToKey._2))).toSet
  }

//  println()
//  println("KeySet")
//  keysSet.foreach(
//    key => println(Base58.encode(key.data))
//  )

  //Get old boxIds list for each of the above public keys
  var keysToBoxIds: Map[ByteArrayWrapper, Seq[Array[Byte]]] = keysSet.map(
    publicKey => publicKey -> boxIdsByKey(publicKey.data)
  ).toMap

  //For each box in temporary map match against public key and remove/append to boxIds list
  for((boxId, publicKey) <- boxesToRemove) {
    keysToBoxIds += (ByteArrayWrapper(publicKey) -> keysToBoxIds(ByteArrayWrapper(publicKey)).filterNot(_ sameElements boxId))
  }
  for((boxId, publicKey) <- boxesToAppend) {
    keysToBoxIds += (ByteArrayWrapper(publicKey) -> (keysToBoxIds(ByteArrayWrapper(publicKey)) :+ boxId))
  }

  bfrStore.update(
    ByteArrayWrapper(newVersion),
    Seq(),
    keysToBoxIds.map(
      element =>
        element._1 -> ByteArrayWrapper(element._2.flatten.toArray)
    ).toSeq)

  BFR(bfrStore, stateStore)
}

  def rollbackTo(version: VersionTag, stateStore: LSMStore): Try[BFR] = Try {
    if (bfrStore.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rolling back BFR to: ${Base58.encode(version)}")
      bfrStore.rollback(ByteArrayWrapper(version))
      BFR(bfrStore, stateStore)
    }
  }

}

object BFR extends ScorexLogging {

  def apply(s1: LSMStore, s2: LSMStore) : BFR = {
    new BFR (s1, s2)
  }

  def readOrGenerate(settings: ForgingSettings, stateStore: LSMStore): BFR = {
    val dataDirOpt = settings.bfrDirOpt.ensuring(_.isDefined, "bfr dir must be specified")
    val dataDir = dataDirOpt.get
    val logDirOpt = settings.logDirOpt
    readOrGenerate(dataDir, logDirOpt, settings, stateStore)
  }

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: ForgingSettings, stateStore: LSMStore): BFR = {
    val iFile = new File(s"$dataDir/map")
    iFile.mkdirs()
    val bfrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing bfr storage...")
        bfrStore.close()
      }
    })

    BFR(bfrStore, stateStore)
  }

}
