package co.topl.storage.graph

import akka.stream.scaladsl.Source
import akka.{Done, NotUsed}
import cats.Foldable
import cats.data.{EitherT, NonEmptyChain}

import scala.concurrent.Future

object BlockchainOps {

  trait Error
  case object NotFound extends Error
  case class ThrowableError(throwable: Throwable) extends Error
  case class ErrorThrowable(error: Error) extends Throwable

}

trait BlockchainOpsProvider {
  import scala.language.implicitConversions
  implicit def headerOps(header:           BlockHeader): BlockHeaderOps
  implicit def bodyOps(body:               BlockBody): BlockBodyOps
  implicit def transactionOps(transaction: Transaction): TransactionOps
  implicit def boxOps(box:                 Box): BoxOps
  implicit def stateOps(state:             State): StateOps

  implicit def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps
  implicit def stringOps(value: String): StringOps

  implicit def blockchainOps(blockchain: Blockchain.type): BlockchainOps
}

trait BlockHeaderOps {
  protected def instance: BlockHeader
  def parentBlock: EitherT[Future, BlockchainOps.Error, BlockHeader]
  def childBlocks: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed]
  def body: EitherT[Future, BlockchainOps.Error, BlockBody]
  def history: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed]
}

trait BlockBodyOps {
  protected def instance: BlockBody
  def header: EitherT[Future, BlockchainOps.Error, BlockHeader]
  def transactions: Source[Either[BlockchainOps.Error, Transaction], NotUsed]
  def state: EitherT[Future, BlockchainOps.Error, State]

  def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainOps.Error, Box]
  def stateChanges: EitherT[Future, BlockchainOps.Error, BlockStateChange]
}

case class BlockStateChange(boxesOpened: Set[String], boxesCreated: Set[String]) {

  def merge(other: BlockStateChange): BlockStateChange = {
    val newOpened = boxesOpened ++ other.boxesOpened
    BlockStateChange(newOpened, (boxesCreated ++ other.boxesCreated) -- newOpened)
  }

  def hash: Array[Byte] = ???
}

trait TransactionOps {
  protected def instance: Transaction
  def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody]
  def opens: Source[Either[BlockchainOps.Error, Box], NotUsed]
  def creates: Source[Either[BlockchainOps.Error, Box], NotUsed]
}

trait BoxOps {
  protected def instance: Box
  def createdBy: EitherT[Future, BlockchainOps.Error, Transaction]
  def openedBy: Source[Either[BlockchainOps.Error, Transaction], NotUsed]
}

trait StateOps {
  protected def instance: State
  def unopenedBoxes: Source[Either[BlockchainOps.Error, Box], NotUsed]

  def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainOps.Error, Box]
}

trait BlockchainModificationsOps {
  def run(): EitherT[Future, BlockchainOps.Error, Done]
}

trait StringOps {
  protected def instance: String
  def blockHeader: EitherT[Future, BlockchainOps.Error, BlockHeader]
  def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody]
  def transaction: EitherT[Future, BlockchainOps.Error, Transaction]
  def box: EitherT[Future, BlockchainOps.Error, Box]
  def addressAccount: EitherT[Future, BlockchainOps.Error, Account]
}

trait BlockchainOps {
  def currentHead: EitherT[Future, BlockchainOps.Error, BlockHeader]
  def currentHeads: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed]
  def genesis: EitherT[Future, BlockchainOps.Error, BlockHeader]
}

sealed trait BlockchainModification

case class SetHead(blockId: String) extends BlockchainModification
case class CreateState(blockId: String) extends BlockchainModification

case class CreateBlockHeader(header: BlockHeader) extends BlockchainModification
case class CreateBlockBody(body: BlockBody) extends BlockchainModification
case class CreateTransaction(transaction: Transaction) extends BlockchainModification
case class CreateBox(box: Box) extends BlockchainModification

case class AssociateBlockToParent(childBlockId: String, parentBlockId: String) extends BlockchainModification
case class AssociateBodyToHeader(headerId: String, bodyId: String) extends BlockchainModification
case class AssociateTransactionToBody(transactionId: String, blockId: String, index: Int) extends BlockchainModification
case class AssociateBoxCreator(boxId: String, transactionId: String, minted: Boolean) extends BlockchainModification
case class AssociateBoxOpener(boxId: String, transactionId: String, attestation: String) extends BlockchainModification

object Blockchain
