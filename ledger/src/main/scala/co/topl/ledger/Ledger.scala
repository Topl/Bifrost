package co.topl.ledger

import cats.implicits._
import co.topl.codecs.bytes.BasicCodecs._
import co.topl.codecs.bytes.ByteCodec.implicits._
import co.topl.codecs.bytes.ByteCodecInstances._
import co.topl.models._
import co.topl.typeclasses.Chainable
import co.topl.typeclasses.Chainable.ops._
import co.topl.typeclasses.ChainableInstances._
import co.topl.typeclasses.Identifiable.ops._
import co.topl.typeclasses.IdentifiableInstances._
import co.topl.typeclasses.ContainsTransactions.ops._
import co.topl.typeclasses.ContainsTransactions.Instances._

import scala.collection.immutable.ArraySeq

case class Ledger(
  private val boxPersistence:         Persistence,
  private val addressPersistence:     Persistence,
  private val blockPersistence:       Persistence,
  private val transactionPersistence: Persistence,
  private val metaPersistence:        Persistence,
  currentHead:                        Block
) {

  def withBlock(block: Block): Either[Ledger.Failure, Ledger] =
    Either
      .cond(block.canChainTo(currentHead), block, Ledger.InvalidBlockParentType(block, currentHead))
      .map(block =>
        applyStateModifications(block)
          .applyTransactionModifications(block)
          .applyBlockModifications(block)
          .applyMetaModifications(block)
          .copy(currentHead = block)
      )

  private def applyStateModifications(block: Block): Ledger = ???
  private def applyBlockModifications(block: Block): Ledger = ???

  private def applyTransactionModifications(block: Block): Ledger =
    copy(
      transactionPersistence = transactionPersistence.write(
        block.id.bytes,
        block.transactions.map(tx => tx.id.bytes -> Some(tx.bytes))
      )
    )

  private def applyMetaModifications(block: Block): Ledger = {
    val idBytes = block.id.bytes
    copy(
      metaPersistence = metaPersistence.write(idBytes, List(Ledger.BestBlockIdKey -> Some(idBytes)))
    )
  }

  def rollback(): Either[Ledger.Failure, Ledger] = {
    val parentId = Chainable[Block].parentId(currentHead)
    getBlock(parentId)
      .toRight(Ledger.NonExistentParent(parentId))
      .map { parent =>
        val parentIdBytes = parentId.bytes
        Ledger(
          boxPersistence.rollbackTo(parentIdBytes),
          addressPersistence.rollbackTo(parentIdBytes),
          blockPersistence.rollbackTo(parentIdBytes),
          transactionPersistence.rollbackTo(parentIdBytes),
          metaPersistence.rollbackTo(parentIdBytes),
          parent
        )
      }
  }

  def getBlock(blockId: TypedIdentifier): Option[Block] =
    blockPersistence.read(List(blockId.bytes)).headOption.flatMap(_._2).map(_.decoded[Block])

  def getTransaction(transactionId: TypedIdentifier): Option[Transaction] =
    blockPersistence.read(List(transactionId.bytes)).headOption.flatMap(_._2).map(_.decoded[Transaction])

  def getBox(boxId: TypedIdentifier): Option[Box] =
    blockPersistence.read(List(boxId.bytes)).headOption.flatMap(_._2).map(_.decoded[Box])

  def boxesForAddress(address: Address): Either[Ledger.Failure, List[Box]] =
    boxesForAddress(address.bytes)

  def boxesForAddress(address: TaktikosAddress): Either[Ledger.Failure, List[Box]] =
    boxesForAddress(address.bytes)

  private def boxesForAddress(bytes: Bytes): Either[Ledger.Failure, List[Box]] =
    addressPersistence
      .read(List(bytes))
      .headOption
      .flatMap(_._2)
      .map(_.decoded[List[TypedIdentifier]])
      .getOrElse(Nil)
      .traverse(id => getBox(id).toRight(Ledger.BoxNotFound(id)))
}

object Ledger {
  sealed abstract class Failure
  case class ParentHeadMismatch(currentHeadId: TypedIdentifier, newBlockId: TypedIdentifier) extends Failure
  case class NonExistentParent(parentBlockId: TypedIdentifier) extends Failure
  case class BoxNotFound(boxId: TypedIdentifier) extends Failure
  case class InvalidBlockParentType(block: Block, parentBlock: Block) extends Failure

  final val BestBlockIdKey: Bytes = new ArraySeq.ofByte(Array.fill(32)(0: Byte))
}
