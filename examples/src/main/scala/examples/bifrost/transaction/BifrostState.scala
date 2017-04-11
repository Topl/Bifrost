package examples.bifrost.transaction

import java.io.File
import java.nio.ByteBuffer

import com.google.common.primitives.Longs
import examples.bifrost.transaction.{PublicKey25519NoncedBox, PublicKey25519NoncedBoxSerializer}
import examples.bifrost.transaction.SimpleState.EmptyVersion
import io.iohk.iodb.{ByteArrayWrapper, LSMStore}
import scorex.core.settings.Settings
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.authenticated.BoxMinimalState
import scorex.core.transaction.state.{MinimalState, StateChanges}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Success, Try}

/**
  * Maintains current state
  * @param version    the identifier for this state
  * @param storage    the underlying datastore of state
  */
case class BifrostState(override val version: VersionTag = EmptyVersion,
                        storage: LSMStore)
  extends BoxMinimalState[PublicKey25519Proposition, PublicKey25519NoncedBox, BifrostTransaction, BifrostBlock, BifrostState] with ScorexLogging {

  def isEmpty: Boolean = version sameElements EmptyVersion

  override def semanticValidity(tx: BifrostTransaction): Try[Unit] = BifrostState.semanticValidity(tx)

  private def lastVersionString = storage.lastVersionID.map(v => Base58.encode(v.data)).getOrElse("None")

  override def boxesOf(p: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] = ???

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    storage.get(ByteArrayWrapper(boxId))
      .map(_.data)
      .map(PublicKey25519NoncedBoxSerializer.parseBytes)
      .flatMap(_.toOption)

  override def rollbackTo(version: VersionTag): Try[BifrostState] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }

  override def applyChanges(change: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[BifrostState] = Try {

    val boxIdsToRemove = change.boxIdsToRemove.map(ByteArrayWrapper.apply)
    val boxesToAdd = change.toAppend.map(b => ByteArrayWrapper(b.id) -> ByteArrayWrapper(b.bytes))

    log.debug(s"Update HBoxStoredState from version $lastVersionString to version ${Base58.encode(newVersion)}. " +
      s"Removing boxes with ids ${boxIdsToRemove.map(b => Base58.encode(b.data))}, " +
      s"adding boxes ${boxesToAdd.map(b => Base58.encode(b._1.data))}")

    if (storage.lastVersionID.isDefined) boxIdsToRemove.foreach(i => require(closedBox(i.data).isDefined))
      storage.update(ByteArrayWrapper(newVersion), boxIdsToRemove, boxesToAdd)

    val newSt = BifrostState(newVersion, storage)
    boxIdsToRemove.foreach(box => require(newSt.closedBox(box.data).isEmpty, s"Box $box is still in state"))
    newSt

  }

  override type NVCT = BifrostState

  override def validate(transaction: BifrostTransaction): Try[Unit] = transaction match {
    case bp: BifrostPayment => Try {
      val b = boxesOf(bp.sender).head
      (b.value >= Math.addExact(bp.amount, bp.fee)) && (b.nonce + 1 == bp.nonce)
    }
    case cc: ContractCreation => Try {
      // TODO check coin is possessed by investor
      // TODO check agreement is valid
      // TODO check reputation of parties
    }
    case a: Agreement => Try {
      // TODO check reputations of parties
      // TODO check terms, pledge, etc.
    }

  }

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  def changes(transaction: BifrostTransaction): Try[TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    transaction match {
      case bp: BifrostPayment if !isEmpty => Try {

        val oldSenderBox = boxesOf(bp.sender).head
        val oldRecipientBox = boxesOf(bp.recipient).headOption

        val newRecipientBox = oldRecipientBox.map { oldB =>
          oldB.copy(nonce = oldB.nonce + 1, value = Math.addExact(oldB.value, bp.amount))
        }.getOrElse(PublicKey25519NoncedBox(bp.recipient, 0L, bp.amount))

        val newSenderBox = oldSenderBox.copy(nonce = oldSenderBox.nonce + 1,
          value = Math.addExact(Math.addExact(oldSenderBox.value, -bp.amount), -bp.fee))

        val toRemove = Set(oldSenderBox) ++ oldRecipientBox
        val toAppend = Set(newRecipientBox, newSenderBox).ensuring(_.forall(_.value >= 0))

        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend, bp.fee)
      }
      case genesisBP: BifrostPayment if isEmpty => Try {
        val toAppend: Set[PublicKey25519NoncedBox] = Set(PublicKey25519NoncedBox(genesisBP.recipient, 0L, genesisBP.amount))
        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), toAppend, 0)
      }
      case cc: ContractCreation => Try {
        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), Set(), 0)
      }
      case _ => Failure(new Exception("implementation is needed"))
    }
  }

  override def changes(block: BifrostBlock): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = Try {
    // TODO eliminate reward?
    val generatorReward = block.txs.map(_.fee).sum
    val gen = block.generator

    val txChanges = block.txs.map(tx => changes(tx)).map(_.get)
    val toRemove = txChanges.flatMap(_.toRemove).map(_.id).toSet
    val toAppendFrom = txChanges.flatMap(_.toAppend).toSet
    val (generator, withoutGenerator) = toAppendFrom.partition(_.proposition.address == gen.address)
    val generatorBox: PublicKey25519NoncedBox = (generator ++ boxesOf(gen)).headOption match {
      case Some(oldBox) =>
        oldBox.copy(nonce = oldBox.nonce + 1, value = oldBox.value + generatorReward)
      case None =>
        PublicKey25519NoncedBox(gen, 1, generatorReward)
    }
    val toAppend = withoutGenerator + generatorBox
    require(toAppend.forall(_.value >= 0))

    StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend)
  }
}



object BifrostState {

  def semanticValidity(tx: BifrostTransaction): Try[Unit] = Try {
   /* require(tx.from.size == tx.signatures.size)
    require(tx.to.forall(_._2 >= 0))
    require(tx.fee >= 0)
    require(tx.timestamp >= 0)
    require(tx.from.zip(tx.signatures).forall { case ((prop, _), proof) =>
      proof.isValid(prop, tx.messageToSign)
    })*/
  }


  def changes(mod: BifrostBlock): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
      Try {
        val initial = (Set(): Set[Array[Byte]], Set(): Set[PublicKey25519NoncedBox], 0L)

        val (toRemove: Set[Array[Byte]], toAdd: Set[PublicKey25519NoncedBox], reward) =
          mod.transactions.map(_.foldLeft(initial) { case ((sr, sa, f), tx) =>
            (sr ++ tx.boxIdsToOpen.toSet, sa ++ tx.newBoxes.toSet, f + tx.fee)
          }).getOrElse((Set(), Set(), 0L)) //no reward additional to tx fees

        StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAdd)
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

    BifrostState(Array.emptyByteArray, stateStorage)
  }

  def genesisState(settings: Settings, initialBlocks: Seq[BifrostBlock]): BifrostState = {
    initialBlocks.foldLeft(readOrGenerate(settings)) { (state, mod) =>
      state.changes(mod).flatMap(cs => state.applyChanges(cs, mod.id)).get
    }
  }
}