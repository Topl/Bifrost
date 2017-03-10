package examples.bifrost.transaction

import java.nio.ByteBuffer

import examples.curvepos.transaction.SimpleState.EmptyVersion
import scorex.core.transaction.box.Box
import scorex.core.transaction.box.proposition.{Proposition, PublicKey25519Proposition}
import scorex.core.transaction.state.MinimalState.VersionTag
import scorex.core.transaction.state.{MinimalState, StateChanges}
import scorex.core.utils.ScorexLogging
import scorex.crypto.encode.Base58

import scala.util.{Failure, Try}

case class BifrostState(override val version: VersionTag = EmptyVersion,
                       storage: Map[ByteBuffer, PublicKey25519NoncedBox] = Map()) extends ScorexLogging
  with MinimalState[PublicKey25519Proposition, PublicKey25519NoncedBox, BifrostTransaction, BifrostBlock, BifrostState] {

  def isEmpty: Boolean = version sameElements EmptyVersion

  def totalBalance: Long = storage.keySet.flatMap(k => storage.get(k).map(_.value)).sum

  override def toString: String = {
    s"BifrostState at ${Base58.encode(version)}\n" + storage.keySet.flatMap(k => storage.get(k)).mkString("\n  ")
  }

  override def boxesOf(p: PublicKey25519Proposition): Seq[PublicKey25519NoncedBox] =
    storage.values.filter(b => b.proposition.address == p.address).toSeq

  override def closedBox(boxId: Array[Byte]): Option[PublicKey25519NoncedBox] =
    storage.get(ByteBuffer.wrap(boxId))

  override def rollbackTo(version: VersionTag): Try[BifrostState] = {
    log.warn("Rollback is not implemented")
    Try(this)
  }

  override def applyChanges(change: StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox],
                            newVersion: VersionTag): Try[BifrostState] = Try {

    val rmap = change.boxIdsToRemove.foldLeft(storage) { case (m, id) => m - ByteBuffer.wrap(id) }

    val amap = change.toAppend.foldLeft(rmap) { case (m, b) =>
      require(b.value >= 0)
      m + (ByteBuffer.wrap(b.id) -> b)
    }
    BifrostState(newVersion, amap)
  }

  override type NVCT = BifrostState

  override def validate(transaction: BifrostTransaction): Try[Unit] = transaction match {
    case sp: BifrostPayment => Try {
      val b = boxesOf(sp.sender).head
      (b.value >= Math.addExact(sp.amount, sp.fee)) && (b.nonce + 1 == sp.nonce)
    }
  }

  /**
    * A Transaction opens existing boxes and creates new ones
    */
  def changes(transaction: BifrostTransaction): Try[TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = {
    transaction match {
      case tx: BifrostPayment if !isEmpty => Try {
        val oldSenderBox = boxesOf(tx.sender).head
        val oldRecipientBox = boxesOf(tx.recipient).headOption
        val newRecipientBox = oldRecipientBox.map { oldB =>
          oldB.copy(nonce = oldB.nonce + 1, value = Math.addExact(oldB.value, tx.amount))
        }.getOrElse(PublicKey25519NoncedBox(tx.recipient, 0L, tx.amount))
        val newSenderBox = oldSenderBox.copy(nonce = oldSenderBox.nonce + 1,
          value = Math.addExact(Math.addExact(oldSenderBox.value, -tx.amount), -tx.fee))
        val toRemove = Set(oldSenderBox) ++ oldRecipientBox
        val toAppend = Set(newRecipientBox, newSenderBox).ensuring(_.forall(_.value >= 0))

        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](toRemove, toAppend, tx.fee)
      }
      case genesis: BifrostPayment if isEmpty => Try {
        val toAppend: Set[PublicKey25519NoncedBox] = Set(PublicKey25519NoncedBox(genesis.recipient, 0L, genesis.amount))
        TransactionChanges[PublicKey25519Proposition, PublicKey25519NoncedBox](Set(), toAppend, 0)
      }
      case _ => Failure(new Exception("implementation is needed"))
    }
  }

  override def changes(block: BifrostBlock): Try[StateChanges[PublicKey25519Proposition, PublicKey25519NoncedBox]] = Try {
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
  val EmptyVersion: Array[Byte] = Array.fill(32)(0: Byte)
}