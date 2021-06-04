package co.topl.storage.graph

import akka.{Done, NotUsed}
import akka.stream.scaladsl.Source
import cats.data.{EitherT, NonEmptyChain}

import scala.concurrent.Future

import scala.languageFeature.implicitConversions
import scala.language.implicitConversions

object BlockchainOps {

  trait Error
  case object NotFound extends Error
  case class ThrowableError(throwable: Throwable) extends Error

}

trait BlockchainOpsProvider {
  implicit def headerOps(header:           BlockHeader): BlockHeaderOps
  implicit def bodyOps(body:               BlockBody): BlockBodyOps
  implicit def transactionOps(transaction: Transaction): TransactionOps
  implicit def boxOps(box:                 Box): BoxOps

  implicit def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps
  implicit def stringOps(value: String): StringOps

  implicit def blockchainOps(blockchain: Blockchain.type): BlockchainOps
}

trait BlockHeaderOps {
  def parentBlock: EitherT[Future, BlockchainOps.Error, BlockHeader]
  def childBlocks: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed]
  def body: EitherT[Future, BlockchainOps.Error, BlockBody]
  def history: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed]
}

trait BlockBodyOps {
  def header: EitherT[Future, BlockchainOps.Error, BlockHeader]
  def transactions: Source[Either[BlockchainOps.Error, Transaction], NotUsed]
}

trait TransactionOps {
  def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody]
  def opens: Source[Either[BlockchainOps.Error, Box], NotUsed]
  def creates: Source[Either[BlockchainOps.Error, Box], NotUsed]
}

trait BoxOps {
  def createdBy: EitherT[Future, BlockchainOps.Error, Transaction]
  def openedBy: Source[Either[BlockchainOps.Error, Transaction], NotUsed]
}

trait BlockchainModificationsOps {
  def run(): EitherT[Future, BlockchainOps.Error, Done]
}

trait StringOps {
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
