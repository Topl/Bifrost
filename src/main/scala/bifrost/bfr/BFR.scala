package bifrost.bfr

import java.io.File

import bifrost.blocks.BifrostBlock
import bifrost.forging.ForgingSettings
import bifrost.state.BifrostState
import bifrost.transaction.box._
import bifrost.transaction.box.proposition.PublicKey25519Proposition
import bifrost.utils.ScorexLogging
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.crypto.encode.Base58

import scala.util.Try

class BFR(store: LSMStore, state: BifrostState) extends ScorexLogging {

  def boxIdsByKey(publicKey: PublicKey25519Proposition): Seq[Array[Byte]] =
    boxIdsByKey(publicKey.pubKeyBytes)

  def boxIdsByKey(pubKeyBytes: Array[Byte]): Seq[Array[Byte]] = store
    .get(ByteArrayWrapper(pubKeyBytes))
    .map(_
      .data
      .grouped(store.keySize)
      .toSeq)
    .getOrElse(Seq[Array[Byte]]())

  def boxesByKey(publicKey: PublicKey25519Proposition): Seq[BifrostBox] =
    boxesByKey(publicKey.pubKeyBytes)

  def boxesByKey(pubKeyBytes: Array[Byte]): Seq[BifrostBox] = {
    boxIdsByKey(pubKeyBytes)
      .map(id => state.closedBox(id))
      .filter {
        case box: Some[BifrostBox] => true
        case None => false
      }
      .map(_.get)
  }

  //noinspection ScalaStyle
  def scanPersistent(modifier: BifrostBlock): BFR = {
    log.debug(s"Applying modifier to BFR: ${Base58.encode(modifier.id)}")
    val changes = BifrostState.changes(modifier).get
    val newState: BifrostState = state.applyModifier(modifier).get

    var boxesToRemove: Map[Array[Byte], Array[Byte]] = Map()
    var boxesToAppend: Map[Array[Byte], Array[Byte]] = Map()
    //Getting set of public keys for boxes being removed and appended
    val keysSet: Set[Array[Byte]] = {
      val boxesToRemoveKeys = changes.boxIdsToRemove
        .flatMap(boxId => state.closedBox(boxId))
        .filter(box => box.isInstanceOf[ArbitBox] || box.isInstanceOf[AssetBox] || box.isInstanceOf[PolyBox])
        .map(box => box match {
            //Filtering token boxes (these are the only boxes whose propositions are assured to be publicKeys)
            //Adding other boxes to BFR needs box rearchitecture so that propositions match those of token boxes
            case arbit: ArbitBox => {
              boxesToRemove += (arbit.id -> arbit.proposition.pubKeyBytes)
              //boxesToRemove = boxesToRemove :+ arbit
              arbit.proposition.pubKeyBytes
            }
            case asset: AssetBox => {
              //boxesToRemove = boxesToRemove :+ asset
              boxesToRemove += (asset.id -> asset.proposition.pubKeyBytes)
              asset.proposition.pubKeyBytes
            }
            case poly: PolyBox => {
              boxesToRemove += (poly.id -> poly.proposition.pubKeyBytes)
              //boxesToRemove = boxesToRemove :+ poly
              poly.proposition.pubKeyBytes
            }
          })

      val boxesToAppendKeys: Set[Array[Byte]] = changes.toAppend
        .filter(box => box.isInstanceOf[ArbitBox] || box.isInstanceOf[AssetBox] || box.isInstanceOf[PolyBox])
//        .map(box => box match {
        .map({
          case arbit: ArbitBox => {
            boxesToAppend += (arbit.id -> arbit.proposition.pubKeyBytes)
            arbit.proposition.pubKeyBytes
          }
          case asset: AssetBox => {
            boxesToAppend += (asset.id -> asset.proposition.pubKeyBytes)
            asset.proposition.pubKeyBytes
          }
          case poly: PolyBox => {
            boxesToAppend += (poly.id -> poly.proposition.pubKeyBytes)
            poly.proposition.pubKeyBytes
          }
        })
      boxesToRemoveKeys.union(boxesToAppendKeys)
    }

    //Get old boxIds list for each of the above public keys
    var keysToBoxIds: Map[Array[Byte], Seq[Array[Byte]]] = keysSet.map(
      publicKey => publicKey -> boxIdsByKey(publicKey)
    ).toMap

    //For each box in temporary map match against public key and remove/append to boxIds list
    for((boxId, publicKey) <- boxesToRemove) {
      keysToBoxIds += publicKey -> keysToBoxIds(publicKey).filterNot(_ sameElements boxId)
    }
    for((boxId, publicKey) <- boxesToAppend) {
      keysToBoxIds += publicKey -> (keysToBoxIds(publicKey) :+ boxId)
    }

    store.update(
      ByteArrayWrapper(modifier.id),
      Seq(),
      keysToBoxIds.map(
        element =>
          ByteArrayWrapper(element._1) -> ByteArrayWrapper(element._2.flatten.toArray)
      ).toSeq)

    BFR(store, newState).get
  }

}

object BFR extends ScorexLogging {

//  def apply(s: BFRStorage, state: BifrostState) : Try[BFR] = Try {
//    new BFR(Map[ByteArrayWrapper, ByteArrayWrapper](), s)
//  }
  def apply(s: LSMStore, state: BifrostState) : Try[BFR] = Try {
    new BFR (s, state)
  }

  def readOrGenerate(settings: ForgingSettings, state: BifrostState): BFR = {
    val dataDirOpt = settings.bfrDirOpt.ensuring(_.isDefined, "bfr dir must be specified")
    val dataDir = dataDirOpt.get
    val logDirOpt = settings.logDirOpt
    readOrGenerate(dataDir, logDirOpt, settings, state)
  }

  def readOrGenerate(dataDir: String, logDirOpt: Option[String], settings: ForgingSettings, state: BifrostState): BFR = {
    val iFile = new File(s"$dataDir/map")
    iFile.mkdirs()
    val bfrStore = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        log.info("Closing bfr storage...")
        bfrStore.close()
      }
    })

//    val storage = new BFRStorage(bfrStorage)
    BFR(bfrStore, state).get
  }

}
