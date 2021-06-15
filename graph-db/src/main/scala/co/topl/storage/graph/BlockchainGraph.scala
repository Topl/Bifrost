package co.topl.storage.graph

import akka.actor.typed.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.codecs.implicits.identityBytesEncoder
import co.topl.utils.encode.Base58
import com.orientechnologies.orient.core.exception.OConcurrentModificationException

import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.language.implicitConversions

class BlockchainGraph(val graph: OrientDBGraph)(implicit system: ActorSystem[_]) extends BlockchainData {

  import BlockchainGraph._
  import system.executionContext

  initialize()

  implicit private def g: OrientDBGraph = graph

  implicit private def opsProvider: BlockchainData = this

  implicit override def headerOps(header: BlockHeader): BlockHeaderOps =
    new BlockHeaderOps {

      override protected def instance: BlockHeader = header

      override def parentBlock: EitherT[Future, BlockchainData.Error, BlockHeader] =
        Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
          .out(BlockParent.edgeSchema)
          .single()

      override def childBlocks: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
          .in(BlockParent.edgeSchema)
          .source

      override def body: EitherT[Future, BlockchainData.Error, BlockBody] =
        Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
          .in(BodyHeader.edgeSchema)
          .single()

      override def history: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        Traverse(NodesByClass[BlockHeader](where = PropEquals("blockId", header.blockId)))
          .out(BlockParent.edgeSchema)
          .source
          .drop(1)
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new BlockBodyOps {

      override protected def instance: BlockBody = body

      override def header: EitherT[Future, BlockchainData.Error, BlockHeader] =
        Trace[BlockBody](where = PropEquals("blockId", body.blockId))
          .out(BodyHeader.edgeSchema)
          .single()

      override def transactions: Source[Either[BlockchainData.Error, Transaction], NotUsed] =
        Trace[BlockBody](where = PropEquals("blockId", body.blockId))
          .in(TransactionBlock.edgeSchema)
          .source

      override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box] =
        state
          .flatMap(_.lookupUnopenedBox(boxId))
          .recoverWith { case BlockchainData.NotFound =>
            stateChanges.flatMap(blockStateChanges =>
              if (blockStateChanges.boxesOpened.contains(boxId)) EitherT.leftT[Future, Box](BlockchainData.NotFound)
              else if (blockStateChanges.boxesCreated.contains(boxId)) boxId.box
              // Recursively call on the parent
              else header.flatMap(_.parentBlock.flatMap(_.body.flatMap(body => bodyOps(body).lookupUnopenedBox(boxId))))
            )
          }

      override def stateChanges: EitherT[Future, BlockchainData.Error, BlockStateChange] =
        EitherT(
          transactions
            .map(_.getOrThrow(BlockchainData.ErrorThrowable))
            .runFoldAsync(BlockStateChange(Set.empty, Set.empty)) { case (changes, transaction) =>
              for {
                opens <- transaction.opens
                  .map(_.getOrThrow(BlockchainData.ErrorThrowable).boxId)
                  .runWith(Sink.seq)
                creates <- transaction.creates
                  .map(_.getOrThrow(BlockchainData.ErrorThrowable).boxId)
                  .runWith(Sink.seq)
              } yield BlockStateChange(changes.boxesOpened ++ opens, changes.boxesCreated ++ creates)
            }
            .map(Right(_))
            .recover {
              case BlockchainData.ErrorThrowable(e) => Left(e)
              case e                                => Left(BlockchainData.ThrowableError(e))
            }
        )

      override def state: EitherT[Future, BlockchainData.Error, State] =
        Trace[BlockBody](where = PropEquals("blockId", body.blockId))
          .out(BodyState.edgeSchema)
          .single()
    }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      override protected def instance: Transaction = transaction

      override def blockBody: EitherT[Future, BlockchainData.Error, BlockBody] =
        Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
          .out(TransactionBlock.edgeSchema)
          .single()

      override def opens: Source[Either[BlockchainData.Error, Box], NotUsed] =
        Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
          .out(TransactionBoxOpens.edgeSchema)
          .source

      override def creates: Source[Either[BlockchainData.Error, Box], NotUsed] =
        Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
          .out(TransactionBoxCreates.edgeSchema)
          .source
    }

  implicit override def boxOps(box: Box): BoxOps =
    new BoxOps {

      override protected def instance: Box = box

      override def createdBy: EitherT[Future, BlockchainData.Error, Transaction] =
        Trace[Box](where = PropEquals("boxId", box.boxId))
          .in(TransactionBoxCreates.edgeSchema)
          .single()

      override def openedBy: Source[Either[BlockchainData.Error, Transaction], NotUsed] =
        Trace[Box](where = PropEquals("boxId", box.boxId))
          .in(TransactionBoxOpens.edgeSchema)
          .source
    }

  implicit override def stringOps(value: String): StringOps =
    new StringOps {

      override protected def instance: String = value

      override def blockHeader: EitherT[Future, BlockchainData.Error, BlockHeader] =
        NodesByClass[BlockHeader](where = PropEquals("blockId", value)).single()

      override def blockBody: EitherT[Future, BlockchainData.Error, BlockBody] =
        NodesByClass[BlockBody](where = PropEquals("blockId", value)).single()

      override def transaction: EitherT[Future, BlockchainData.Error, Transaction] =
        NodesByClass[Transaction](where = PropEquals("transactionId", value)).single()

      override def box: EitherT[Future, BlockchainData.Error, Box] =
        NodesByClass[Box](where = PropEquals("boxId", value)).single()

      override def addressAccount: EitherT[Future, BlockchainData.Error, Account] =
        NodesByClass[Account](where = PropEquals("address", value)).single()

      /**
       * Retrieve the state associated with this String ID
       */
      override def state: EitherT[Future, BlockchainData.Error, State] =
        NodesByClass[State](where = PropEquals("stateId", value)).single()
    }

  implicit override def blockchainOps(blockchain: Blockchain.type): BlockchainOps =
    new BlockchainOps {

      override def currentHead: EitherT[Future, BlockchainData.Error, BlockHeader] =
        Trace[ChainHead]().out(CanonicalHead.edgeSchema).single()

      override def currentHeads: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        NodesByClass[BlockHeader](where = PropEquals(s"in('${BlockParent.edgeSchema.name}').size()", 0)).source

      override def genesis: EitherT[Future, BlockchainData.Error, BlockHeader] =
        NodesByClass[BlockHeader](where = PropEquals(s"out('${BlockParent.edgeSchema.name}').size()", 0)).single()

      override def blocksAtHeight(height: Long): Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        NodesByClass[BlockHeader](where = PropEquals("height", height)).source
    }

  implicit override def stateOps(state: State): StateOps =
    new StateOps {
      override protected def instance: State = state

      override def unopenedBoxes: Source[Either[BlockchainData.Error, Box], NotUsed] =
        Trace[State](where = PropEquals("stateId", state.stateId))
          .out(StateUnopenedBox.edgeSchema)
          .source

      override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box] =
        Raw[Box](
          s"SELECT $$box" +
          s" FROM ${Transaction.nodeSchema.name}" +
          s" LET $$box=expand(out('${StateUnopenedBox.edgeSchema.name}'))" +
          s" WHERE stateId='${state.stateId}'" +
          s" AND $$box.boxId='$boxId'"
        )
          .single()
    }

  implicit override def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps =
    new BlockchainModificationsOps {

      /**
       * Run the chain modifications in-order
       */
      override def run(): EitherT[Future, BlockchainData.Error, Done] =
        EitherT(
          graph
            .transactionally { implicit session =>
              val support = new BlockchainGraphModificationSupport()
              Source(modifications.toNonEmptyList.toList)
                .mapAsync(1) {
                  case c: CreateBlockHeader =>
                    support.apply(c).value
                  case c: CreateBlockBody =>
                    support.apply(c).value
                  case c: CreateTransaction =>
                    support.apply(c).value
                  case c: CreateBox =>
                    support.apply(c).value
                  case c: SetHead =>
                    support.apply(c).value
                  case c: AssociateBlockToParent =>
                    support.apply(c).value
                  case c: AssociateBodyToHeader =>
                    support.apply(c).value
                  case c: AssociateTransactionToBody =>
                    support.apply(c).value
                  case c: AssociateBoxCreator =>
                    support.apply(c).value
                  case c: AssociateBoxOpener =>
                    support.apply(c).value
                  case c: CreateState =>
                    support.apply(c).value
                }
                .takeWhile(_.isRight, inclusive = true)
                .runWith(Sink.last)
            }
            .recover { case _: OConcurrentModificationException => Left(BlockchainData.OrientDBConcurrencyError) }
        )
          .recoverWith { case BlockchainData.OrientDBConcurrencyError => run() }
    }

  private def initialize(): Unit =
    Await.result(
      graph
        .getNode[ChainHead](NodesByClass[ChainHead]())
        .valueOrF { case OrientDBGraph.ThrowableError(throwable) => Future.failed(throwable) }
        .flatMap {
          case Some(_) => Future.successful(Done)
          case _ =>
            graph.transactionally(
              _.insertNode(ChainHead())
                .valueOrF { case OrientDBGraph.ThrowableError(throwable) => Future.failed(throwable) }
            )
        },
      2.minutes
    )

}

object BlockchainGraph {

  implicit class QueryOps[T: NodeSchema](query: GraphQuery[T]) {

    def single()(implicit graph: ReadableGraph, ec: ExecutionContext): EitherT[Future, BlockchainData.Error, T] =
      graph
        .getNode(query)
        .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
        .subflatMap(_.toRight(BlockchainData.NotFound))

    def source(implicit graph: ReadableGraph): Source[Either[BlockchainData.Error, T], NotUsed] =
      graph
        .getNodes(query)
        .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
        .takeWhile(_.isRight, inclusive = true)
  }
}

object BlockchainGraphStateSupport {

  trait Ops {

    implicit class BlockBodyStateOps(blockBody: BlockBody) {

      def createState(implicit
        opsProvider:   BlockchainData,
        ec:            ExecutionContext,
        orientDBGraph: WritableGraph,
        mat:           Materializer
      ): EitherT[Future, BlockchainData.Error, Done] = {
        import opsProvider._
        blockBody.state
          .map(_ => Done)
          .recoverWith { case BlockchainData.NotFound =>
            EitherT(
              (for {
                blockStateHistory <-
                  traverseToNearestState
                    .concat(Source.single(blockBody))
                    .mapAsync(1)(b =>
                      b.stateChanges
                        .map((b.blockId, _))
                        .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(e)))
                    )
                    .runWith(Sink.seq)
                    .map(_.reverse)
                closestState <-
                  blockStateHistory.head._1.blockBody.flatMap(_.state).value.map {
                    case Right(s)                      => Some(s)
                    case Left(BlockchainData.NotFound) => None
                    case Left(e)                       => throw BlockchainData.ErrorThrowable(e)
                  }
                historyAfterClosestState =
                  blockStateHistory.drop(closestState.size.toInt)
                newStateId = historyAfterClosestState
                  .map(_._2.hash)
                  .foldLeft(
                    closestState.fold(Array.fill(32)(0: Byte))(cs => Base58.decode(cs.stateId).getOrThrow())
                  ) { case (previousStateHash, changeHistoryHash) =>
                    co.topl.crypto.hash.blake2b256.hash(None, previousStateHash, changeHistoryHash).value
                  }
                encodedStateId = Base58.encode(newStateId)
                _ <- orientDBGraph
                  .insertNode[State](State(encodedStateId))
                  .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(BlockchainData.OrientDBGraphError(e))))

                knownUnopenedBoxes = {
                  val mergedStateChange =
                    historyAfterClosestState
                      .map(_._2)
                      .fold(BlockStateChange(Set.empty, Set.empty))(_.combine(_))
                  mergedStateChange.boxesCreated -- mergedStateChange.boxesOpened
                }
                _ <-
                  orientDBGraph
                    .insertEdge(
                      BodyState(),
                      OrientDBGraph.NodeReference(
                        NodesByClass[BlockBody](where = PropEquals("blockId", blockBody.blockId))
                      ),
                      OrientDBGraph.NodeReference(
                        NodesByClass[State](where = PropEquals("stateId", encodedStateId))
                      )
                    )
                    .valueOrF(e => Future.failed(BlockchainData.ErrorThrowable(BlockchainData.OrientDBGraphError(e))))
                _ <-
                  Source(knownUnopenedBoxes)
                    .concat(closestState.fold(Source.empty[String])(_.unopenedBoxes.map(_.getOrThrow().boxId)))
                    .mapAsync(1)(boxId =>
                      orientDBGraph
                        .insertEdge(
                          StateUnopenedBox(),
                          OrientDBGraph.NodeReference(
                            NodesByClass[State](where = PropEquals("stateId", encodedStateId))
                          ),
                          OrientDBGraph.NodeReference(
                            NodesByClass[Box](where = PropEquals("boxId", boxId))
                          )
                        )
                        .valueOrF(e =>
                          Future.failed(BlockchainData.ErrorThrowable(BlockchainData.OrientDBGraphError(e)))
                        )
                    )
                    .runWith(Sink.ignore)
              } yield Done)
                .map(Right(_))
                .recover { case e => Left(BlockchainData.ThrowableError(e)) }
            )
          }
      }
        .map(_ => Done)

      def traverseToNearestState(implicit
        opsProvider: BlockchainData,
        ec:          ExecutionContext
      ): Source[BlockBody, NotUsed] = {
        import opsProvider._
        Source.unfoldAsync(blockBody)(body =>
          body.state
            .map(_ => None: Option[(BlockBody, BlockBody)])
            .recoverWith { case BlockchainData.NotFound =>
              body.header
                .flatMap(_.parentBlock)
                .biflatMap(
                  {
                    case BlockchainData.NotFound => EitherT.rightT[Future, BlockchainData.Error](None)
                    case e                       => EitherT.leftT[Future, Option[(BlockBody, BlockBody)]](e)
                  },
                  _.body.map(b => Some((b, b)))
                )
            }
            .valueOrF((BlockchainData.ErrorThrowable.apply _).andThen(Future.failed))
        )
      }
    }
  }

  object implicits extends Ops
}

class BlockchainGraphModificationSupport()(implicit
  blockchainData:   BlockchainData,
  orientDBGraph:    WritableGraph,
  executionContext: ExecutionContext,
  materializer:     Materializer
) {
  import blockchainData._

  def apply(createState: CreateState): EitherT[Future, BlockchainData.Error, Done] = {
    import BlockchainGraphStateSupport.implicits._
    createState.blockId.blockBody
      .flatMap(_.createState)
  }

  def apply(createBlockHeader: CreateBlockHeader): EitherT[Future, BlockchainData.Error, Done] =
    orientDBGraph.insertNode(createBlockHeader.header).leftMap(BlockchainData.OrientDBGraphError)

  def apply(createBlockBody: CreateBlockBody): EitherT[Future, BlockchainData.Error, Done] =
    orientDBGraph.insertNode(createBlockBody.body).leftMap(BlockchainData.OrientDBGraphError)

  def apply(createTransaction: CreateTransaction): EitherT[Future, BlockchainData.Error, Done] =
    orientDBGraph.insertNode(createTransaction.transaction).leftMap(BlockchainData.OrientDBGraphError)

  def apply(createBox: CreateBox): EitherT[Future, BlockchainData.Error, Done] =
    orientDBGraph.insertNode(createBox.box).leftMap(BlockchainData.OrientDBGraphError)

  def apply(setHead: SetHead): EitherT[Future, BlockchainData.Error, Done] =
    orientDBGraph
      .deleteEdges[CanonicalHead]()
      .flatMap(_ =>
        orientDBGraph.insertEdge(
          CanonicalHead(),
          OrientDBGraph.NodeReference(NodesByClass[ChainHead]()),
          OrientDBGraph.NodeReference(NodesByClass[BlockHeader](where = PropEquals("blockId", setHead.blockId)))
        )
      )
      .leftMap(BlockchainData.OrientDBGraphError)

  def apply(associateBlockToParent: AssociateBlockToParent): EitherT[Future, BlockchainData.Error, Done] = {
    val parentHeaderReference =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.parentBlockId))
      )
    val childHeader =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.childBlockId))
      )

    orientDBGraph
      .insertEdge(BlockParent(), childHeader, parentHeaderReference)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateBodyToHeader: AssociateBodyToHeader): EitherT[Future, BlockchainData.Error, Done] = {
    val headerId = associateBodyToHeader.headerId
    val bodyId = associateBodyToHeader.bodyId
    val header =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockHeader](where = PropEquals("blockId", headerId))
      )
    val body =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockBody](where = PropEquals("blockId", bodyId))
      )
    orientDBGraph
      .insertEdge(BodyHeader(), body, header)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateTransactionToBody: AssociateTransactionToBody): EitherT[Future, BlockchainData.Error, Done] = {
    val transactionId = associateTransactionToBody.transactionId
    val blockId = associateTransactionToBody.blockId
    val transaction =
      OrientDBGraph.NodeReference(
        NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
      )
    val body =
      OrientDBGraph.NodeReference(
        NodesByClass[BlockBody](where = PropEquals("blockId", blockId))
      )
    orientDBGraph
      .insertEdge(TransactionBlock(), transaction, body)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateBoxCreator: AssociateBoxCreator): EitherT[Future, BlockchainData.Error, Done] = {
    val transactionId = associateBoxCreator.transactionId
    val boxId = associateBoxCreator.boxId

    val transaction =
      OrientDBGraph.NodeReference(
        NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
      )
    val box =
      OrientDBGraph.NodeReference(
        NodesByClass[Box](where = PropEquals("boxId", boxId))
      )
    orientDBGraph
      .insertEdge(TransactionBoxCreates(minted = true), transaction, box)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

  def apply(associateBoxOpener: AssociateBoxOpener): EitherT[Future, BlockchainData.Error, Done] = {
    val transactionId = associateBoxOpener.transactionId
    val boxId = associateBoxOpener.boxId

    val transaction =
      OrientDBGraph.NodeReference(
        NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
      )
    val box =
      OrientDBGraph.NodeReference(
        NodesByClass[Box](where = PropEquals("boxId", boxId))
      )
    orientDBGraph
      .insertEdge(TransactionBoxOpens(), transaction, box)
      .leftMap(BlockchainData.OrientDBGraphError)
  }

}
