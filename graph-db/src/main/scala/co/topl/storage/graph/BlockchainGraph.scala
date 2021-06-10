package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.codecs.implicits.identityBytesEncoder
import co.topl.utils.encode.Base58
import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.impls.orient.OrientVertex

import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions

class BlockchainGraph(val orientDBGraph: OrientDBGraph)(implicit system: ActorSystem) extends BlockchainData {

  import BlockchainGraphModificationSupport.implicits._
  import system.dispatcher

  implicit private def g: OrientDBGraph = orientDBGraph

  implicit private def opsProvider: BlockchainData = this

  implicit override def headerOps(header: BlockHeader): BlockHeaderOps =
    new BlockHeaderOps {

      override protected def instance: BlockHeader = header

      override def parentBlock: EitherT[Future, BlockchainData.Error, BlockHeader] =
        orientDBGraph
          .getNode[BlockHeader](
            Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
              .out(BlockParent.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainData.NotFound))

      override def childBlocks: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .getNodes[BlockHeader](
            Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
              .in(BlockParent.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
          .recover { case e => Left(BlockchainData.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def body: EitherT[Future, BlockchainData.Error, BlockBody] =
        orientDBGraph
          .getNode[BlockBody](
            Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
              .in(BodyHeader.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainData.NotFound))

      override def history: Source[Either[BlockchainData.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .getNodes[BlockHeader](
            Traverse(NodesByClass[BlockHeader](where = PropEquals("blockId", header.blockId)))
              .out(BlockParent.edgeSchema)
          )
          .drop(1)
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new BlockBodyOps {

      override protected def instance: BlockBody = body

      override def header: EitherT[Future, BlockchainData.Error, BlockHeader] =
        orientDBGraph
          .getNode[BlockHeader](
            Trace[BlockBody](where = PropEquals("blockId", body.blockId))
              .out(BodyHeader.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainData.NotFound))

      override def transactions: Source[Either[BlockchainData.Error, Transaction], NotUsed] =
        orientDBGraph
          .getNodes[Transaction](
            Trace[BlockBody](where = PropEquals("blockId", body.blockId))
              .in(TransactionBlock.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)

      override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainData.Error, Box] =
        state
          .flatMap(_.lookupUnopenedBox(boxId))
          .recoverWith { case BlockchainData.NotFound =>
            stateChanges.flatMap(state =>
              if (state.boxesOpened.contains(boxId)) EitherT.leftT[Future, Box](BlockchainData.NotFound)
              else if (state.boxesCreated.contains(boxId)) boxId.box
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
        orientDBGraph
          .getNode[State](
            Trace[BlockBody](where = PropEquals("blockId", body.blockId))
              .out(BodyState.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainData.NotFound))
    }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      override protected def instance: Transaction = transaction

      override def blockBody: EitherT[Future, BlockchainData.Error, BlockBody] =
        orientDBGraph
          .getNode(
            Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
              .out(TransactionBlock.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainData.NotFound))

      override def opens: Source[Either[BlockchainData.Error, Box], NotUsed] =
        orientDBGraph
          .getNodes(
            Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
              .out(TransactionBoxOpens.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)

      override def creates: Source[Either[BlockchainData.Error, Box], NotUsed] =
        orientDBGraph
          .getNodes(
            Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
              .out(TransactionBoxCreates.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)
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
  ): BlockchainModificationsOps = () =>
    EitherT(
      Source(modifications.toNonEmptyList.toList)
        .runFoldAsync(BlockchainGraph.ModificationsState(Nil, Nil)) { case (state, modification) =>
          modification match {
            case c: CreateBlockHeader =>
              state(c)
            case c: CreateBlockBody =>
              state(c)
            case c: CreateTransaction =>
              state(c)
            case c: CreateBox =>
              state(c)
            case c: SetHead =>
              state(c)
            case c: AssociateBlockToParent =>
              state(c)
            case c: AssociateBodyToHeader =>
              state(c)
            case c: AssociateTransactionToBody =>
              state(c)
            case c: AssociateBoxCreator =>
              state(c)
            case c: AssociateBoxOpener =>
              state(c)
            case c: CreateState =>
              state(c)
          }
        }
        .map(_ => Right(Done))
        .recover { case e => Left(BlockchainData.ThrowableError(e)) }
    )

  implicit class QueryOps[T: NodeSchema](query: GraphQuery[T]) {

    def single(): EitherT[Future, BlockchainData.Error, T] =
      orientDBGraph
        .getNode(query)
        .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) }
        .subflatMap(_.toRight(BlockchainData.NotFound))

    def source: Source[Either[BlockchainData.Error, T], NotUsed] =
      orientDBGraph
        .getNodes(query)
        .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainData.ThrowableError(throwable) })
        .takeWhile(_.isRight, inclusive = true)
  }
}

object BlockchainGraph {

  private[graph] case class ModificationsState(newVertices: List[OrientVertex], newEdges: List[Edge]) {
    def withVertex(v: OrientVertex): ModificationsState = copy(newVertices :+ v)
    def withEdge(e:   Edge): ModificationsState = copy(newEdges = newEdges :+ e)
  }
}

object BlockchainGraphStateSupport {

  trait Ops {

    implicit class BlockBodyStateOps(blockBody: BlockBody) {

      def createState(state: BlockchainGraph.ModificationsState)(implicit
        opsProvider:         BlockchainData,
        ec:                  ExecutionContext,
        orientDBGraph:       OrientDBGraph,
        mat:                 Materializer
      ): EitherT[Future, BlockchainData.Error, BlockchainGraph.ModificationsState] = {
        import opsProvider._
        blockBody.state
          .map(_ => state)
          .recoverWith { case BlockchainData.NotFound =>
            EitherT.right(
              for {
                blockStateHistory <-
                  traverseToNearestState
                    .concat(Source.single(blockBody))
                    .mapAsync(1)(b =>
                      b.stateChanges.map((b.blockId, _)).value.map(_.getOrThrow(BlockchainData.ErrorThrowable))
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
                stateNode <-
                  orientDBGraph.insertNode[State](State(Base58.encode(newStateId)))
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
                      OrientDBGraph.QueryNodeReference(
                        NodesByClass[BlockBody](where = PropEquals("blockId", blockBody.blockId))
                      ),
                      OrientDBGraph.VertexNodeReference(stateNode)
                    )
                _ <-
                  Source(knownUnopenedBoxes)
                    .concat(closestState.fold(Source.empty[String])(_.unopenedBoxes.map(_.getOrThrow().boxId)))
                    .runWith(
                      Sink.foreachAsync(1)(boxId =>
                        orientDBGraph
                          .insertEdge(
                            StateUnopenedBox(),
                            OrientDBGraph.VertexNodeReference(stateNode),
                            OrientDBGraph.QueryNodeReference(
                              NodesByClass[Box](where = PropEquals("boxId", boxId))
                            )
                          )
                          .map { _ => }
                      )
                    )
              } yield state.withVertex(stateNode)
            )
          }
      }

      def traverseToNearestState(implicit
        opsProvider: BlockchainData,
        ec:          ExecutionContext
      ): Source[BlockBody, NotUsed] = {
        import opsProvider._
        Source
          .future(
            blockBody.header.value
              .map(_.getOrThrow(BlockchainData.ErrorThrowable))
          )
          .flatMapConcat(
            _.history
              .map(_.getOrThrow(BlockchainData.ErrorThrowable))
              .mapAsync(1)(
                _.body.value
                  .map(_.getOrThrow(BlockchainData.ErrorThrowable))
              )
              .mapAsync(1)(body =>
                body.state
                  .map(Left(_): Either[State, BlockBody])
                  .recover { case BlockchainData.NotFound =>
                    Right(body)
                  }
                  .value
                  .map(_.getOrThrow(BlockchainData.ErrorThrowable))
              )
              .takeWhile(_.isRight)
              .map(_.getOrThrow())
          )
      }
    }
  }

  object implicits extends Ops
}

object BlockchainGraphModificationSupport {

  trait Ops {

    implicit class ModificationsStateOps(state: BlockchainGraph.ModificationsState)(implicit
      blockchainData:                           BlockchainData,
      orientDBGraph:                            OrientDBGraph,
      executionContext:                         ExecutionContext,
      materializer:                             Materializer
    ) {
      import blockchainData._

      def apply(createState: CreateState): Future[BlockchainGraph.ModificationsState] = {
        import BlockchainGraphStateSupport.implicits._
        createState.blockId.blockBody
          .flatMap(_.createState(state))
          .value
          .map(_.getOrThrow(BlockchainData.ErrorThrowable))
      }

      def apply(
        createBlockHeader: CreateBlockHeader
      ): Future[BlockchainGraph.ModificationsState] =
        orientDBGraph.insertNode(createBlockHeader.header).map(state.withVertex)

      def apply(
        createBlockBody: CreateBlockBody
      ): Future[BlockchainGraph.ModificationsState] =
        orientDBGraph.insertNode(createBlockBody.body).map(state.withVertex)

      def apply(
        createTransaction: CreateTransaction
      ): Future[BlockchainGraph.ModificationsState] =
        orientDBGraph.insertNode(createTransaction.transaction).map(state.withVertex)

      def apply(
        createBox: CreateBox
      ): Future[BlockchainGraph.ModificationsState] =
        orientDBGraph.insertNode(createBox.box).map(state.withVertex)

      def apply(
        setHead: SetHead
      ): Future[BlockchainGraph.ModificationsState] = {
        import orientDBGraph.Ops._
        for {
          headVertex <- orientDBGraph
            .getRawNode(NodesByClass[ChainHead]())
            .value
            .map(_.getOrThrow())
            .flatMap {
              case Some(vertex) => Future.successful(vertex)
              case _            => orientDBGraph.insertNode(ChainHead())
            }
          newTarget <- orientDBGraph
            .getRawNode(NodesByClass[BlockHeader](where = PropEquals("blockId", setHead.blockId)))
            .subflatMap(_.toRight(BlockchainData.NotFound))
            .value
            .map(_.getOrThrow())
          _ <- headVertex
            .allEdges(Some(CanonicalHead.edgeSchema.name))
            .runWith(Sink.foreachAsync(1)(_.removeAsync().map { _ => }))
          edge <- orientDBGraph.insertEdge(
            CanonicalHead(),
            OrientDBGraph.VertexNodeReference(headVertex),
            OrientDBGraph.VertexNodeReference(newTarget)
          )
        } yield state.withEdge(edge)
      }

      def apply(
        associateBlockToParent: AssociateBlockToParent
      ): Future[BlockchainGraph.ModificationsState] = {
        val parentHeaderReference = state.newVertices
          .find(v =>
            v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == associateBlockToParent.parentBlockId
          )
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.parentBlockId))
            )
          )
        val childHeader = state.newVertices
          .find(v =>
            v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == associateBlockToParent.childBlockId
          )
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.childBlockId))
            )
          )

        orientDBGraph.insertEdge(BlockParent(), childHeader, parentHeaderReference).map(state.withEdge)
      }

      def apply(
        associateBodyToHeader: AssociateBodyToHeader
      ): Future[BlockchainGraph.ModificationsState] = {
        val headerId = associateBodyToHeader.headerId
        val bodyId = associateBodyToHeader.bodyId
        val header = state.newVertices
          .find(v => v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == headerId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[BlockHeader](where = PropEquals("blockId", headerId))
            )
          )
        val body = state.newVertices
          .find(v => v.getLabel == "BlockBody" && v.getProperty[String]("blockId") == bodyId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[BlockBody](where = PropEquals("blockId", bodyId))
            )
          )
        orientDBGraph.insertEdge(BodyHeader(), body, header).map(state.withEdge)
      }

      def apply(
        associateTransactionToBody: AssociateTransactionToBody
      ): Future[BlockchainGraph.ModificationsState] = {
        val transactionId = associateTransactionToBody.transactionId
        val blockId = associateTransactionToBody.blockId
        val transaction = state.newVertices
          .find(v => v.getLabel == "Transaction" && v.getProperty[String]("transactionId") == transactionId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
            )
          )
        val body = state.newVertices
          .find(v => v.getLabel == "BlockBody" && v.getProperty[String]("blockId") == blockId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[BlockBody](where = PropEquals("blockId", blockId))
            )
          )
        orientDBGraph.insertEdge(TransactionBlock(), transaction, body).map(state.withEdge)
      }

      def apply(
        associateBoxCreator: AssociateBoxCreator
      ): Future[BlockchainGraph.ModificationsState] = {

        val transactionId = associateBoxCreator.transactionId
        val boxId = associateBoxCreator.boxId

        val transaction = state.newVertices
          .find(v => v.getLabel == "Transaction" && v.getProperty[String]("transactionId") == transactionId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
            )
          )
        val box = state.newVertices
          .find(v => v.getLabel == "Box" && v.getProperty[String]("boxId") == boxId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[Box](where = PropEquals("boxId", boxId))
            )
          )
        orientDBGraph.insertEdge(TransactionBoxCreates(minted = true), transaction, box).map(state.withEdge)
      }

      def apply(
        associateBoxOpener: AssociateBoxOpener
      ): Future[BlockchainGraph.ModificationsState] = {
        val transactionId = associateBoxOpener.transactionId
        val boxId = associateBoxOpener.boxId

        val transaction = state.newVertices
          .find(v => v.getLabel == "Transaction" && v.getProperty[String]("transactionId") == transactionId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[Transaction](where = PropEquals("transactionId", transactionId))
            )
          )
        val box = state.newVertices
          .find(v => v.getLabel == "Box" && v.getProperty[String]("boxId") == boxId)
          .map(OrientDBGraph.VertexNodeReference)
          .getOrElse(
            OrientDBGraph.QueryNodeReference(
              NodesByClass[Box](where = PropEquals("boxId", boxId))
            )
          )
        orientDBGraph
          .insertEdge(TransactionBoxOpens(attestation = associateBoxOpener.attestation), transaction, box)
          .map(state.withEdge)
      }
    }
  }

  object implicits extends Ops
}
