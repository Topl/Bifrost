package examples.bifrost.state

import java.io.File

import com.google.common.primitives.Longs
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.transaction._
import examples.bifrost.transaction.box.{BifrostBox, BifrostBoxSerializer, BifrostPaymentBox, PublicKey25519NoncedBox}
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.settings.Settings
import scorex.core.transaction.box.proposition.PublicKey25519Proposition
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.StateChanges
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

case class BifrostTransactionChanges(toRemove: Set[BifrostBox], toAppend: Set[BifrostBox], minerReward: Long)

case class BifrostStateChanges(override val boxIdsToRemove: Set[Array[Byte]],
                               override val toAppend: Set[BifrostBox])
  extends GenericStateChanges[Any, PublicKey25519Proposition, BifrostBox](boxIdsToRemove, toAppend)

case class BifrostState(storage: LSMStore, override val version: VersionTag)
  extends GenericBoxMinimalState[Any, PublicKey25519Proposition, BifrostBox, BifrostTransaction, BifrostBlock, BifrostState] with ScorexLogging {

  override type NVCT = BifrostState
  type P = BifrostState.P
  type T = BifrostState.T
  type TX = BifrostState.TX
  type BX = BifrostState.BX
  type BPMOD = BifrostState.BPMOD
  type GSC = BifrostState.GSC
  type BSC = BifrostState.BSC

  override def semanticValidity(tx: BifrostTransaction): Try[Unit] = BifrostState.semanticValidity(tx)

  private def lastVersionString = storage.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

  override def boxesOf(p: P): Seq[BX] = ???

  override def closedBox(boxId: Array[Byte]): Option[BX] =
    storage.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BifrostBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  override def rollbackTo(version: VersionTag): Try[NVCT] = Try {
    if (storage.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback HBoxStoredState to ${Base58.encode(version)} from version $lastVersionString")
      storage.rollback(ByteArrayWrapper(version))
      BifrostState(storage, version)
    }
  }

  override def changes(mod: BPMOD): Try[GSC] = BifrostState.changes(mod)

  override def applyChanges(changes: GSC, newVersion: VersionTag): Try[NVCT] = Try {

    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)
    val boxesToAdd = changes.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.debug(s"Update HBoxStoredState from version $lastVersionString to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")

    if (storage.lastVersionID.isDefined) boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))
    storage.update(ByteArrayWrapper(newVersion), boxIdsToRemove, boxesToAdd)

    val newSt = BifrostState(storage, newVersion)
    boxIdsToRemove.foreach(box => require(newSt.closedBox(box.data).isEmpty, s"Box $box is still in state"))
    newSt

  }

  override def validate(transaction: TX): Try[Unit] = transaction match {
    case bp: StableCoinTransfer => Try {
      val b = boxesOf(bp.from.head._1).head.asInstanceOf[BifrostPaymentBox]
      b.value >= Math.addExact(bp.to.foldLeft(0)((a, b) => a + b._2.toInt), bp.fee)// && (b.nonce + 1 == bp.nonce)
    }
    case cc: ContractCreation => Try {
      // TODO check coin is possessed by investor
      // TODO check agreement is valid
      // TODO check reputation of parties
    }
  }

}

object BifrostState {

  type T = Any
  type TX = BifrostTransaction
  type P = PublicKey25519Proposition
  type BX = BifrostBox
  type BPMOD = BifrostBlock
  type GSC = GenericStateChanges[T, P, BX]
  type BSC = BifrostStateChanges

  def semanticValidity(tx: TX): Try[Unit] = Try {
    tx match {
      case sc: StableCoinTransfer => StableCoinTransfer.validate(sc)
      case cc: ContractCreation => ContractCreation.validate(cc)
      case _ => throw new Exception("Semantic validity not implemented for " + tx.getClass.toGenericString)
    }
  }


  def changes(mod: BPMOD): Try[GSC] = {
    Try {
      val initial = (Set(): Set[Array[Byte]], Set(): Set[BX], 0L)

      val boxDeltas = mod.transactions.map {
        // (rm, add, fee)
        case sc: StableCoinTransfer => (sc.boxIdsToOpen.toSet, sc.newBoxes.toSet, sc.fee)
        case cc: ContractCreation => (cc.boxIdsToOpen.toSet, cc.newBoxes.toSet, cc.fee)
      }

      val (toRemove: Set[Array[Byte]], toAdd: Set[BX], reward: Long) =
        boxDeltas.foldLeft((Set[Array[Byte]](), Set[BX](), 0L))((aggregate, boxDelta) => {
          (aggregate._1 ++ boxDelta._1, aggregate._2 ++ boxDelta._2, aggregate._3 + boxDelta._3 )
        })

      val forgerNonce = Longs.fromByteArray(mod.id.take(Longs.BYTES))
      val forgerBox = BifrostPaymentBox(mod.generator, forgerNonce, reward)

      //no reward additional to tx fees
      BifrostStateChanges(toRemove, toAdd)
    }
  }

  def readOrGenerate(settings: Settings): BifrostState = {
    val dataDirOpt = settings.dataDirOpt.ensuring(_.isDefined, "data dir must be specified")
    val dataDir = dataDirOpt.get

    new File(dataDir).mkdirs()

    val iFile = new File(s"$dataDir/state")
    iFile.mkdirs()
    val stateStorage = new LSMStore(iFile)

    Runtime.getRuntime.addShutdownHook(new Thread() {
      override def run(): Unit = {
        stateStorage.close()
      }
    })
    val version = stateStorage.lastVersionID.map(_.data).getOrElse(Array.emptyByteArray)

    BifrostState(stateStorage, version)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[BPMOD]): BifrostState = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, mod.id)).get
    }
  }
}