package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.stream.scaladsl.{Sink, Source}
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import com.tinkerpop.blueprints.Edge
import com.tinkerpop.blueprints.impls.orient.OrientVertex

import scala.concurrent.Future
import scala.language.implicitConversions

class BlockchainGraph(val orientDBGraph: OrientDBGraph)(implicit system: ActorSystem) extends BlockchainOpsProvider {

  import orientDBGraph.Ops._
  import system.dispatcher

  implicit override def headerOps(header: BlockHeader): BlockHeaderOps =
    new BlockHeaderOps {

      override def parentBlock: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          orientDBGraph
            .getNode[BlockHeader](
              s"SELECT expand(out('${BlockParent.edgeSchema.name}')) FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${header.blockId}'"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def childBlocks: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"SELECT expand(in('${BlockParent.edgeSchema.name}')) FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${header.blockId}'"
          )
          .map(_.as[BlockHeader])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def body: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          orientDBGraph
            .getNode[BlockBody](
              s"SELECT expand(in('${BodyHeader.edgeSchema.name}')) FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${header.blockId}'"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def history: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"TRAVERSE out('${BlockParent.edgeSchema.name}') FROM (SELECT FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${header.blockId}')"
          )
          .drop(1)
          .map(_.as[BlockHeader])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new BlockBodyOps {

      override def header: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          orientDBGraph
            .getNode[BlockHeader](
              s"SELECT expand(out('${BodyHeader.edgeSchema.name}')) FROM ${BlockBody.nodeSchema.name} WHERE blockId='${body.blockId}'"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def transactions: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"SELECT expand(in('${TransactionBlock.edgeSchema.name}')) FROM ${BlockBody.nodeSchema.name} WHERE blockId='${body.blockId}'"
          )
          .map(_.as[Transaction])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      override def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          orientDBGraph
            .getNode[BlockBody](
              s"SELECT expand(out('${TransactionBlock.edgeSchema.name}')) FROM ${Transaction.nodeSchema.name} WHERE transactionId='${transaction.transactionId}'"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def opens: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"SELECT expand(out('${TransactionBoxOpens.edgeSchema.name}')) FROM ${Transaction.nodeSchema.name} WHERE transactionId='${transaction.transactionId}'"
          )
          .map(_.as[Box])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def creates: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"SELECT expand(out('${TransactionBoxCreates.edgeSchema.name}')) FROM ${Transaction.nodeSchema.name} WHERE transactionId='${transaction.transactionId}'"
          )
          .map(_.as[Box])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def boxOps(box: Box): BoxOps =
    new BoxOps {

      override def createdBy: EitherT[Future, BlockchainOps.Error, Transaction] =
        EitherT(
          orientDBGraph
            .getNode[Transaction](
              s"SELECT expand(in('${TransactionBoxCreates.edgeSchema.name}')) FROM ${Box.nodeSchema.name} WHERE boxId='${box.boxId}'"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def openedBy: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"SELECT expand(in('${TransactionBoxOpens.edgeSchema.name}')) FROM ${Box.nodeSchema.name} WHERE boxId='${box.boxId}'"
          )
          .map(_.as[Transaction])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

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
        .resultSetQuery[OrientVertex](s"SELECT FROM ${ChainHead.nodeSchema.name}")
        .runWith(Sink.headOption)
        .flatMap {
          case Some(vertex) => Future.successful(vertex)
          case _            => orientDBGraph.insertNode(ChainHead())
        }
      newTarget <- orientDBGraph
        .resultSetQuery[OrientVertex](s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${setHead.blockId}'")
        .runWith(Sink.head)
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
          s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${associateBlockToParent.parentBlockId}'"
        )
      )
    val childHeader = state.newVertices
      .find(v => v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == associateBlockToParent.childBlockId)
      .map(OrientDBGraph.VertexNodeReference)
      .getOrElse(
        OrientDBGraph.QueryNodeReference(
          s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE blockId='${associateBlockToParent.childBlockId}'"
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
          s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE blockId='$headerId'"
        )
      )
    val body = state.newVertices
      .find(v => v.getLabel == "BlockBody" && v.getProperty[String]("blockId") == bodyId)
      .map(OrientDBGraph.VertexNodeReference)
      .getOrElse(
        OrientDBGraph.QueryNodeReference(
          s"SELECT FROM ${BlockBody.nodeSchema.name} WHERE blockId='$bodyId'"
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
          s"SELECT FROM ${Transaction.nodeSchema.name} WHERE transactionId='$transactionId'"
        )
      )
    val body = state.newVertices
      .find(v => v.getLabel == "BlockBody" && v.getProperty[String]("blockId") == blockId)
      .map(OrientDBGraph.VertexNodeReference)
      .getOrElse(
        OrientDBGraph.QueryNodeReference(
          s"SELECT FROM ${BlockBody.nodeSchema.name} WHERE blockId='$blockId'"
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
          s"SELECT FROM ${Transaction.nodeSchema.name} WHERE transactionId='$transactionId'"
        )
      )
    val box = state.newVertices
      .find(v => v.getLabel == "Box" && v.getProperty[String]("boxId") == boxId)
      .map(OrientDBGraph.VertexNodeReference)
      .getOrElse(
        OrientDBGraph.QueryNodeReference(
          s"SELECT FROM ${Box.nodeSchema.name} WHERE boxId='$boxId'"
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
          s"SELECT FROM ${Transaction.nodeSchema.name} WHERE transactionId='$transactionId'"
        )
      )
    val box = state.newVertices
      .find(v => v.getLabel == "Box" && v.getProperty[String]("boxId") == boxId)
      .map(OrientDBGraph.VertexNodeReference)
      .getOrElse(
        OrientDBGraph.QueryNodeReference(
          s"SELECT FROM ${Box.nodeSchema.name} WHERE boxId='$boxId'"
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
          }
        }
        .map(_ => Right(Done))
        .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
    )

  implicit override def stringOps(value: String): StringOps =
    new StringOps {

      override def blockHeader: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          orientDBGraph
            .getNode[BlockHeader](s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE blockId='$value'")
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          orientDBGraph
            .getNode[BlockBody](s"SELECT FROM ${BlockBody.nodeSchema.name} WHERE blockId='$value'")
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def transaction: EitherT[Future, BlockchainOps.Error, Transaction] =
        EitherT(
          orientDBGraph
            .getNode[Transaction](s"SELECT FROM ${Transaction.nodeSchema.name} WHERE transactionId='$value'")
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def box: EitherT[Future, BlockchainOps.Error, Box] =
        EitherT(
          orientDBGraph
            .getNode[Box](s"SELECT FROM ${Box.nodeSchema.name} WHERE boxId='$value'")
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def addressAccount: EitherT[Future, BlockchainOps.Error, Account] =
        EitherT(
          orientDBGraph
            .getNode[Account](s"SELECT FROM ${Account.nodeSchema.name} WHERE address='$value'")
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )
    }

  implicit override def blockchainOps(blockchain: Blockchain.type): BlockchainOps =
    new BlockchainOps {

      override def currentHead: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          orientDBGraph
            .getNode[BlockHeader](
              s"SELECT expand(out('${CanonicalHead.edgeSchema.name}')) FROM ${ChainHead.nodeSchema.name}"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def currentHeads: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        orientDBGraph
          .resultSetQuery[OrientVertex](
            s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE in('${BlockParent.edgeSchema.name}').size()=0"
          )
          .map(_.as[BlockHeader])
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def genesis: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          orientDBGraph
            .getNode[BlockHeader](
              s"SELECT FROM ${BlockHeader.nodeSchema.name} WHERE out('${BlockParent.edgeSchema.name}').size()=0"
            )
            .map(_.toRight(BlockchainOps.NotFound))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )
    }
}

object BlockchainGraph {

  private case class ModificationsState(newVertices: List[OrientVertex], newEdges: List[Edge]) {
    def withVertex(v: OrientVertex): ModificationsState = copy(newVertices :+ v)
    def withEdge(e:   Edge): ModificationsState = copy(newEdges = newEdges :+ e)
  }
}
