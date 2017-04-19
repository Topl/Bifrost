package examples.bifrost.state

import java.io.File
import java.time.Instant

import com.google.common.primitives.Longs
import examples.bifrost.blocks.BifrostBlock
import examples.bifrost.transaction._
import examples.bifrost.transaction.box._
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.crypto.hash.FastCryptographicHash
import scorex.core.settings.Settings
import scorex.core.transaction.Transaction
import scorex.core.transaction.box.proposition.{ProofOfKnowledgeProposition, Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{PrivateKey25519, StateChanges}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

case class BifrostTransactionChanges(toRemove: Set[BifrostBox], toAppend: Set[BifrostBox], minerReward: Long)

case class BifrostStateChanges(override val boxIdsToRemove: Set[Array[Byte]],
                               override val toAppend: Set[BifrostBox], timestamp: Long)
  extends GenericStateChanges[Any, ProofOfKnowledgeProposition[PrivateKey25519], BifrostBox](boxIdsToRemove, toAppend)

case class BifrostState(storage: LSMStore, override val version: VersionTag, timestamp: Long)
  extends GenericBoxMinimalState[Any, ProofOfKnowledgeProposition[PrivateKey25519],
    BifrostBox, BifrostTransaction, BifrostBlock, BifrostState] with ScorexLogging {

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

  override def closedBox(boxId: Array[Byte]): Option[BX] =
    storage.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(BifrostBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  override def rollbackTo(version: VersionTag): Try[NVCT] = Try {
    if (storage.lastVersionID.exists(_.data sameElements version)) {
      this
    } else {
      log.debug(s"Rollback BifrostState to ${Base58.encode(version)} from version $lastVersionString")
      storage.rollback(ByteArrayWrapper(version))
      val timestamp: Long = Longs.fromByteArray(storage.get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes))).get.data)
      BifrostState(storage, version, timestamp)
    }
  }

  override def changes(mod: BPMOD): Try[GSC] = BifrostState.changes(mod)

  override def applyChanges(changes: GSC, newVersion: VersionTag): Try[NVCT] = Try {

    val boxIdsToRemove = changes.boxIdsToRemove.map(ByteArrayWrapper.apply)

    // TODO check if b.bytes screws up compared to BifrostBoxCompanion.toBytes
    val boxesToAdd = changes.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.debug(s"Update BifrostState from version $lastVersionString to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")

    val timestamp: Long = changes.asInstanceOf[BifrostStateChanges].timestamp

    if (storage.lastVersionID.isDefined) boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))

    storage.update(
      ByteArrayWrapper(newVersion),
      boxIdsToRemove,
      boxesToAdd + (ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes)) -> ByteArrayWrapper(Longs.toByteArray(timestamp)))
    )

    val newSt = BifrostState(storage, newVersion, timestamp)
    boxIdsToRemove.foreach(box => require(newSt.closedBox(box.data).isEmpty, s"Box $box is still in state"))
    newSt

  }

  override def validate(transaction: TX): Try[Unit] = transaction match {
    case sct: StableCoinTransfer => validateStableCoinTransfer(sct)
    case cc: ContractCreation => validateContractCreation(cc)
  }

  def validateStableCoinTransfer(sct: StableCoinTransfer): Try[Unit] = Try {

    val statefulValid: Try[Unit] = {

      val boxesSumTry: Try[Long] = {
        sct.unlockers.foldLeft[Try[Long]](Success(0L))((partialRes, unlocker) =>

          partialRes.flatMap(partialSum =>
            /* Checks if unlocker is valid and if so adds to current running total */
            closedBox(unlocker.closedBoxId) match {
              case Some(box) =>
                if (unlocker.boxKey.isValid(box.proposition, sct.messageToSign)) {
                  Success(partialSum + box.asInstanceOf[StableCoinBox].value)
                } else {
                  Failure(new Exception("Incorrect unlocker"))
                }
              case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
            }
          )

        )
      }

      boxesSumTry flatMap { openSum =>
        if (sct.newBoxes.map(_.asInstanceOf[StableCoinBox].value).sum == openSum - sct.fee) {
          Success[Unit](Unit)
        } else {
          Failure(new Exception("Negative fee"))
        }
      }

    }

    statefulValid.flatMap(_ => semanticValidity(sct))
  }
  def validateContractCreation(cc: ContractCreation): Try[Unit] = Try {

    val unlockersValid: Try[Unit] = cc.unlockers.foldLeft[Try[Unit]](Success())((unlockersValid, unlocker) =>

      unlockersValid.flatMap { (unlockerValidity) =>
        closedBox(unlocker.closedBoxId) match {
          case Some(box) =>
            if (unlocker.boxKey.isValid(box.proposition, cc.messageToSign)) {
              Success()
            } else {
              Failure(new Exception("Incorrect unlcoker"))
            }
          case None => Failure(new Exception(s"Box for unlocker $unlocker is not in the state"))
        }
      }
    )

    val statefulValid = unlockersValid flatMap { _ =>

      val boxesAreNew = cc.newBoxes.forall(curBox => storage.get(ByteArrayWrapper(curBox.asInstanceOf[ContractBox].id)) match {
        case Some(box) => false
        case None => true
      })

      val txTimestampIsAcceptable = cc.timestamp > timestamp && timestamp < Instant.now().toEpochMilli

      if (boxesAreNew && txTimestampIsAcceptable) {
        Success[Unit](Unit)
      } else {
        Failure(new Exception("Boxes attempt to overwrite existing contract"))
      }
    }

    statefulValid.flatMap(_ => semanticValidity(cc))

    // TODO check reputation of parties
  }

}

object BifrostState {

  type T = Any
  type TX = BifrostTransaction
  type P = ProofOfKnowledgeProposition[PrivateKey25519]
  type BX = BifrostBox
  type BPMOD = BifrostBlock
  type GSC = GenericStateChanges[T, P, BX]
  type BSC = BifrostStateChanges

  def semanticValidity(tx: TX): Try[Unit] = {
    tx match {
      case sc: StableCoinTransfer => StableCoinTransfer.validate(sc)
      case cc: ContractCreation => ContractCreation.validate(cc)
      case _ => Failure( new Exception("Semantic validity not implemented for " + tx.getClass.toGenericString))
    }
  }


  def changes(mod: BPMOD): Try[GSC] = {
    Try {
      val initial = (Set(): Set[Array[Byte]], Set(): Set[BX], 0L)

      val boxDeltas: Seq[(Set[Array[Byte]], Set[BX], Long)] = mod.transactions match {
        case Some(txSeq) => txSeq.map {
          // (rm, add, fee)
          case sc: StableCoinTransfer => (sc.boxIdsToOpen.toSet, sc.newBoxes.toSet, sc.fee)
          case cc: ContractCreation => (cc.boxIdsToOpen.toSet, cc.newBoxes.toSet, cc.fee)
        }
      }

      val (toRemove: Set[Array[Byte]], toAdd: Set[BX], reward: Long) =
        boxDeltas.foldLeft((Set[Array[Byte]](), Set[BX](), 0L))((aggregate, boxDelta) => {
          (aggregate._1 ++ boxDelta._1, aggregate._2 ++ boxDelta._2, aggregate._3 + boxDelta._3 )
        })

      val forgerNonce = Longs.fromByteArray(mod.id.take(Longs.BYTES))
      val forgerBox = StableCoinBox(mod.generator, forgerNonce, reward)

      //no reward additional to tx fees
      BifrostStateChanges(toRemove, toAdd, mod.timestamp)
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

    val timestamp: Long = Longs.fromByteArray(stateStorage.get(ByteArrayWrapper(FastCryptographicHash("timestamp".getBytes))).get.data)

    BifrostState(stateStorage, version, timestamp)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[BPMOD]): BifrostState = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, mod.id)).get
    }
  }
}