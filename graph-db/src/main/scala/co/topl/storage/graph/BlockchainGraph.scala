package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import cats.implicits._
import co.topl.utils.IdiomaticScalaTransition.implicits.toEitherOps
import co.topl.utils.codecs.implicits.identityBytesEncoder
import co.topl.utils.encode.Base58
import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.impls.orient.OrientVertex

import scala.concurrent.Future
import scala.language.implicitConversions

class BlockchainGraph(val orientDBGraph: OrientDBGraph)(implicit system: ActorSystem) extends BlockchainOpsProvider {

  import system.dispatcher

  implicit override def headerOps(header: BlockHeader): BlockHeaderOps =
    new BlockHeaderOps {

      override protected def instance: BlockHeader = header

      override def parentBlock: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        orientDBGraph
          .getNode[BlockHeader](
            Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
              .out(BlockParent.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def childBlocks: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .getNodes[BlockHeader](
            Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
              .in(BlockParent.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def body: EitherT[Future, BlockchainOps.Error, BlockBody] =
        orientDBGraph
          .getNode[BlockBody](
            Trace[BlockHeader](where = PropEquals("blockId", header.blockId))
              .in(BodyHeader.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def history: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .getNodes[BlockHeader](
            Traverse(NodesByClass[BlockHeader](where = PropEquals("blockId", header.blockId)))
              .out(BlockParent.edgeSchema)
          )
          .drop(1)
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new BlockBodyOps {

      override protected def instance: BlockBody = body

      override def header: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        orientDBGraph
          .getNode[BlockHeader](
            Trace[BlockBody](where = PropEquals("blockId", body.blockId))
              .out(BodyHeader.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def transactions: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        orientDBGraph
          .getNodes[Transaction](
            Trace[BlockBody](where = PropEquals("blockId", body.blockId))
              .in(TransactionBlock.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)

      override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainOps.Error, Box] =
        state
          .flatMap(_.lookupUnopenedBox(boxId))
          .recoverWith { case BlockchainOps.NotFound =>
            stateChanges.flatMap(state =>
              if (state.boxesOpened.contains(boxId)) EitherT.leftT[Future, Box](BlockchainOps.NotFound)
              else if (state.boxesCreated.contains(boxId)) boxId.box
              else header.flatMap(_.parentBlock.flatMap(_.body.flatMap(body => bodyOps(body).lookupUnopenedBox(boxId))))
            )
          }

      override def stateChanges: EitherT[Future, BlockchainOps.Error, BlockStateChange] =
        EitherT(
          transactions
            .map(_.getOrThrow(BlockchainOps.ErrorThrowable))
            .runFoldAsync(BlockStateChange(Set.empty, Set.empty)) { case (changes, transaction) =>
              for {
                opens <- transaction.opens
                  .map(_.getOrThrow(BlockchainOps.ErrorThrowable).boxId)
                  .runWith(Sink.seq)
                creates <- transaction.creates
                  .map(_.getOrThrow(BlockchainOps.ErrorThrowable).boxId)
                  .runWith(Sink.seq)
              } yield BlockStateChange(changes.boxesOpened ++ opens, changes.boxesCreated ++ creates)
            }
            .map(Right(_))
            .recover {
              case BlockchainOps.ErrorThrowable(e) => Left(e)
              case e                               => Left(BlockchainOps.ThrowableError(e))
            }
        )

      override def state: EitherT[Future, BlockchainOps.Error, State] =
        orientDBGraph
          .getNode[State](
            Trace[BlockBody](where = PropEquals("blockId", body.blockId))
              .out(BodyState.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))
    }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      override protected def instance: Transaction = transaction

      override def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody] =
        orientDBGraph
          .getNode(
            Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
              .out(TransactionBlock.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def opens: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        orientDBGraph
          .getNodes(
            Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
              .out(TransactionBoxOpens.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)

      override def creates: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        orientDBGraph
          .getNodes(
            Trace[Transaction](where = PropEquals("transactionId", transaction.transactionId))
              .out(TransactionBoxCreates.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def stateOps(state: State): StateOps =
    new StateOps {
      override protected def instance: State = state

      override def unopenedBoxes: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        orientDBGraph
          .getNodes(
            Trace[State](where = PropEquals("stateId", state.stateId))
              .out(StateUnopenedBox.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)

      override def lookupUnopenedBox(boxId: String): EitherT[Future, BlockchainOps.Error, Box] =
        orientDBGraph
          .getNode[Box](
            Raw[Box](
              s"SELECT $$box" +
              s" FROM ${Transaction.nodeSchema.name}" +
              s" LET $$box=expand(out('${StateUnopenedBox.edgeSchema.name}'))" +
              s" WHERE stateId='${state.stateId}'" +
              s" AND $$box.boxId='$boxId'"
            )
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))
    }

  implicit override def boxOps(box: Box): BoxOps =
    new BoxOps {

      override protected def instance: Box = box

      override def createdBy: EitherT[Future, BlockchainOps.Error, Transaction] =
        orientDBGraph
          .getNode(
            Trace[Box](where = PropEquals("boxId", box.boxId))
              .in(TransactionBoxCreates.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def openedBy: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        orientDBGraph
          .getNodes[Transaction](
            Trace[Box](where = PropEquals("boxId", box.boxId))
              .in(TransactionBoxOpens.edgeSchema)
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)
    }

  private def applyCreateState(
    blockId: String,
    state:   BlockchainGraph.ModificationsState
  ): Future[BlockchainGraph.ModificationsState] =
    blockId.blockBody
      .flatMap(body =>
        body.state
          .map(_ => state)
          .recoverWith { case BlockchainOps.NotFound =>
            EitherT.right(
              for {
                blockStateHistory <-
                  traverseToNearestState(body)
                    .concat(Source.single(body))
                    .mapAsync(1)(b =>
                      b.stateChanges.map((b.blockId, _)).value.map(_.getOrThrow(BlockchainOps.ErrorThrowable))
                    )
                    .runWith(Sink.seq)
                    .map(_.reverse)
                closestState <-
                  blockStateHistory.head._1.blockBody.flatMap(_.state).value.map {
                    case Right(s)                     => Some(s)
                    case Left(BlockchainOps.NotFound) => None
                    case Left(e)                      => throw BlockchainOps.ErrorThrowable(e)
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
                        NodesByClass[BlockBody](where = PropEquals("blockId", blockId))
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
      )
      .value
      .map(_.getOrThrow(BlockchainOps.ErrorThrowable))

  private def traverseToNearestState(blockBody: BlockBody): Source[BlockBody, NotUsed] =
    Source
      .future(
        blockBody.header.value
          .map(_.getOrThrow(BlockchainOps.ErrorThrowable))
      )
      .flatMapConcat(
        _.history
          .map(_.getOrThrow(BlockchainOps.ErrorThrowable))
          .mapAsync(1)(
            _.body.value
              .map(_.getOrThrow(BlockchainOps.ErrorThrowable))
          )
          .mapAsync(1)(body =>
            body.state
              .map(Left(_): Either[State, BlockBody])
              .recover { case BlockchainOps.NotFound =>
                Right(body)
              }
              .value
              .map(_.getOrThrow(BlockchainOps.ErrorThrowable))
          )
          .takeWhile(_.isRight)
          .map(_.getOrThrow())
      )

  private def applyCreateBlockHeader(
    createBlockHeader: CreateBlockHeader,
    state:             BlockchainGraph.ModificationsState
  ): Future[BlockchainGraph.ModificationsState] =
    orientDBGraph.insertNode(createBlockHeader.header).map(state.withVertex)

  private def applyCreateBlockBody(
    createBlockBody: CreateBlockBody,
    state:           BlockchainGraph.ModificationsState
  ): Future[BlockchainGraph.ModificationsState] =
    orientDBGraph.insertNode(createBlockBody.body).map(state.withVertex)

  private def applyCreateTransaction(
    createTransaction: CreateTransaction,
    state:             BlockchainGraph.ModificationsState
  ): Future[BlockchainGraph.ModificationsState] =
    orientDBGraph.insertNode(createTransaction.transaction).map(state.withVertex)

  private def applyCreateBox(
    createBox: CreateBox,
    state:     BlockchainGraph.ModificationsState
  ): Future[BlockchainGraph.ModificationsState] =
    orientDBGraph.insertNode(createBox.box).map(state.withVertex)

  private def applySetHead(
    setHead: SetHead,
    state:   BlockchainGraph.ModificationsState
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
        .subflatMap(_.toRight(BlockchainOps.NotFound))
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

  private def applyAssociateBlockToParent(
    associateBlockToParent: AssociateBlockToParent,
    state:                  BlockchainGraph.ModificationsState
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
      .find(v => v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == associateBlockToParent.childBlockId)
      .map(OrientDBGraph.VertexNodeReference)
      .getOrElse(
        OrientDBGraph.QueryNodeReference(
          NodesByClass[BlockHeader](where = PropEquals("blockId", associateBlockToParent.childBlockId))
        )
      )

    orientDBGraph.insertEdge(BlockParent(), childHeader, parentHeaderReference).map(state.withEdge)
  }

  private def applyAssociateBodyToHeader(
    associateBodyToHeader: AssociateBodyToHeader,
    state:                 BlockchainGraph.ModificationsState
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

  private def applyAssociateTransactionToBody(
    associateTransactionToBody: AssociateTransactionToBody,
    state:                      BlockchainGraph.ModificationsState
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

  private def applyAssociateBoxCreator(
    associateBoxCreator: AssociateBoxCreator,
    state:               BlockchainGraph.ModificationsState
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

  private def applyAssociateBoxOpener(
    associateBoxOpener: AssociateBoxOpener,
    state:              BlockchainGraph.ModificationsState
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

  implicit override def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps = () =>
    EitherT(
      Source(modifications.toNonEmptyList.toList)
        .runFoldAsync(BlockchainGraph.ModificationsState(Nil, Nil)) { case (state, modification) =>
          modification match {
            case c: CreateBlockHeader =>
              applyCreateBlockHeader(c, state)
            case c: CreateBlockBody =>
              applyCreateBlockBody(c, state)
            case c: CreateTransaction =>
              applyCreateTransaction(c, state)
            case c: CreateBox =>
              applyCreateBox(c, state)
            case c: SetHead =>
              applySetHead(c, state)
            case c: AssociateBlockToParent =>
              applyAssociateBlockToParent(c, state)
            case c: AssociateBodyToHeader =>
              applyAssociateBodyToHeader(c, state)
            case c: AssociateTransactionToBody =>
              applyAssociateTransactionToBody(c, state)
            case c: AssociateBoxCreator =>
              applyAssociateBoxCreator(c, state)
            case c: AssociateBoxOpener =>
              applyAssociateBoxOpener(c, state)
            case c: CreateState =>
              applyCreateState(c.blockId, state)
          }
        }
        .map(_ => Right(Done))
        .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
    )

  implicit override def stringOps(value: String): StringOps =
    new StringOps {

      override protected def instance: String = value

      override def blockHeader: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        orientDBGraph
          .getNode(
            NodesByClass[BlockHeader](where = PropEquals("blockId", value))
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody] =
        orientDBGraph
          .getNode(
            NodesByClass[BlockBody](where = PropEquals("blockId", value))
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def transaction: EitherT[Future, BlockchainOps.Error, Transaction] =
        orientDBGraph
          .getNode(
            NodesByClass[Transaction](where = PropEquals("transactionId", value))
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def box: EitherT[Future, BlockchainOps.Error, Box] =
        orientDBGraph
          .getNode(
            NodesByClass[Box](where = PropEquals("boxId", value))
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def addressAccount: EitherT[Future, BlockchainOps.Error, Account] =
        orientDBGraph
          .getNode(
            NodesByClass[Account](where = PropEquals("address", value))
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

    }

  implicit override def blockchainOps(blockchain: Blockchain.type): BlockchainOps =
    new BlockchainOps {

      override def currentHead: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        orientDBGraph
          .getNode[BlockHeader](
            Trace[ChainHead]().out(CanonicalHead.edgeSchema)
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))

      override def currentHeads: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .getNodes(
            NodesByClass[BlockHeader](where = PropEquals(s"in('${BlockParent.edgeSchema.name}').size()", 0))
          )
          .map(_.leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) })
          .takeWhile(_.isRight, inclusive = true)

      override def genesis: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        orientDBGraph
          .getNode[BlockHeader](
            NodesByClass[BlockHeader](where = PropEquals(s"out('${BlockParent.edgeSchema.name}').size()", 0))
          )
          .leftMap { case OrientDBGraph.ThrowableError(throwable) => BlockchainOps.ThrowableError(throwable) }
          .subflatMap(_.toRight(BlockchainOps.NotFound))
    }
}

object BlockchainGraph {

  private case class ModificationsState(newVertices: List[OrientVertex], newEdges: List[Edge]) {
    def withVertex(v: OrientVertex): ModificationsState = copy(newVertices :+ v)
    def withEdge(e:   Edge): ModificationsState = copy(newEdges = newEdges :+ e)
  }
}
