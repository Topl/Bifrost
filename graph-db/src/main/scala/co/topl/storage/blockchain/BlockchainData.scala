package co.topl.storage.blockchain

import akka.stream.scaladsl.Source
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import co.topl.storage.generic.GenericStore
import co.topl.storage.graph.OrientDBGraph

import scala.concurrent.Future

/**
 * Provider of an operations DSL for interacting with blockchain data
 */
trait BlockchainData {
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

object BlockchainData {

  trait Error
  case object NotFound extends Error
  case class ThrowableError(throwable: Throwable) extends Error
  case class OrientDBGraphError(error: OrientDBGraph.Error) extends Error
  case object OrientDBConcurrencyError extends Error
  case class GenericStoreError(genericStoreError: GenericStore.Error) extends Error

  case class ErrorThrowable(error: Error) extends Throwable
}

trait BlockHeaderOps {
  protected def instance: BlockHeader

  /**
   * Retrieve the parent block header to this block.
   *
   * Note that if this block is the genesis block, Left[NotFound] is returned
   */
  def parentBlock: EitherT[Future, BlockchainData.Error, BlockHeader]

  /**
   * Retrieve all blocks that are children to this block
   */
  def childBlocks: Source[Either[BlockchainData.Error, BlockHeader], NotUsed]

  /**
   * Retrieve the body associated with this header.
   */
  def body: EitherT[Future, BlockchainData.Error, BlockBody]

  /**
   * Lazily and recursively fetch all parent block headers of this block, excluding this block
   */
  def history: Source[Either[BlockchainData.Error, BlockHeader], NotUsed]
}

trait BlockBodyOps {
  protected def instance: BlockBody

  /**
   * Retrieve the parent block header
   */
  def header: EitherT[Future, BlockchainData.Error, BlockHeader]

  /**
   * Retrieve all transactions within this block
   */
  def transactions: Source[Either[BlockchainData.Error, Transaction], NotUsed]

  /**
   * Retrieve the state node associated with this block body.
   *
   * Note that most block bodies will not have states, so expect a Left[NotFound]
   */
  def state: EitherT[Future, BlockchainData.Error, State]

  /**
   * Lookup a block by ID, but only if it is unopened from this block's perspective
   */
  def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box]

  /**
   * Compute the state changes resulting from this block
   */
  def stateChanges: EitherT[Future, BlockchainData.Error, BlockStateChange]
}

trait TransactionOps {
  protected def instance: Transaction

  /**
   * Fetch the parent block body
   */
  def blockBody: EitherT[Future, BlockchainData.Error, BlockBody]

  /**
   * Fetch all of the boxes opened by this transaction
   */
  def opens: Source[Either[BlockchainData.Error, Box], NotUsed]

  /**
   * Retrieve all of the boxes created by this transaction
   */
  def creates: Source[Either[BlockchainData.Error, Box], NotUsed]
}

trait BoxOps {
  protected def instance: Box

  /**
   * Retrieve the transaction that created this box.
   */
  def createdBy: EitherT[Future, BlockchainData.Error, Transaction]

  /**
   * Retrieve all transactions that open this box.
   *
   * Note that within a single tine, a Box should only ever be opened once.  This method retrieves all transactions
   * that open this box across all tines.
   */
  def openedBy: Source[Either[BlockchainData.Error, Transaction], NotUsed]
}

trait StateOps {
  protected def instance: State

  /**
   * Retrieve all unopened boxes within this State
   */
  def unopenedBoxIds: Source[Either[BlockchainData.Error, String], NotUsed]

  /**
   * Find the box associated with the given ID, but only if the box is unopened within this state
   */
  def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box]
}

trait BlockchainModificationsOps {

  /**
   * Run the chain modifications in-order
   */
  def run(): EitherT[Future, BlockchainData.Error, Done]
}

trait StringOps {
  protected def instance: String

  /**
   * Retrieve the block header associated with this String ID
   */
  def blockHeader: EitherT[Future, BlockchainData.Error, BlockHeader]

  /**
   * Retrieve the block body associated with this String ID
   */
  def blockBody: EitherT[Future, BlockchainData.Error, BlockBody]

  /**
   * Retrieve the transaction associated with this String ID
   */
  def transaction: EitherT[Future, BlockchainData.Error, Transaction]

  /**
   * Retrieve the box associated with this String ID
   */
  def box: EitherT[Future, BlockchainData.Error, Box]

  /**
   * Retrieve the account associated with the address with this String ID
   */
  def addressAccount: EitherT[Future, BlockchainData.Error, Account]

  /**
   * Retrieve the state associated with this String ID
   */
  def state: EitherT[Future, BlockchainData.Error, State]
}

trait BlockchainOps {

  /**
   * Retrieve the "canonical head" of the Blockchain
   */
  def currentHead: EitherT[Future, BlockchainData.Error, BlockHeader]

  /**
   * Retrieve all "heads" of the Blockchain, including those that are non-canonical
   */
  def currentHeads: Source[Either[BlockchainData.Error, BlockHeader], NotUsed]

  /**
   * Retrieve the genesis/starting block of the Blockchain
   */
  def genesis: EitherT[Future, BlockchainData.Error, BlockHeader]

  def blocksAtHeight(height: Long): Source[Either[BlockchainData.Error, BlockHeader], NotUsed]
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
case class AssociateBoxOpener(boxId: String, transactionId: String) extends BlockchainModification

object Blockchain
