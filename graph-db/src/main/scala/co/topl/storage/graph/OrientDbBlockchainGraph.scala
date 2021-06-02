package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorAttributes, Attributes}
import akka.{Done, NotUsed}
import cats.data.{EitherT, NonEmptyChain}
import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient.{
  OrientDynaElementIterable,
  OrientGraphFactory,
  OrientGraphNoTx,
  OrientVertex
}
import com.tinkerpop.blueprints.{Direction, Edge, Vertex}

import java.nio.file.Path
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class OrientDbBlockchainGraph(location: OrientDbBlockchainGraph.Location)(implicit system: ActorSystem)
    extends BlockchainOpsProvider {

  import system.dispatcher

  private var factory: OrientGraphFactory = _
  private var session: OrientGraphNoTx = _

  private val blockingDispatcher = system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)

  initialize()

  import OrientDbBlockchainGraph.implicits._

  implicit override def headerOps(header: BlockHeader): BlockHeaderOps =
    new BlockHeaderOps {

      override def parentBlock: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          resultSetQuery[OrientVertex](
            s"SELECT expand(out('ParentBlock')) FROM BlockHeader WHERE blockId='${header.id}'"
          )
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def childBlocks: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        resultSetQuery[OrientVertex](s"SELECT expand(in('ParentBlock')) FROM BlockHeader WHERE blockId='${header.id}'")
          .map(_.asBlockHeader)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def body: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT expand(in('BodyHeader')) FROM BlockHeader WHERE blockId='${header.id}'")
            .map(_.asBlockBody)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def history: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        resultSetQuery[OrientVertex](
          s"TRAVERSE out('ParentBlock') FROM (SELECT FROM BlockHeader WHERE blockId='${header.id}')"
        )
          .drop(1)
          .map(_.asBlockHeader)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new BlockBodyOps {

      override def header: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT expand(out('BodyHeader')) FROM BlockBody WHERE blockId='${body.id}'")
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def transactions: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        resultSetQuery[OrientVertex](s"SELECT expand(in('TransactionBlock')) FROM BlockBody WHERE blockId='${body.id}'")
          .map(_.asTransaction)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      override def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          resultSetQuery[OrientVertex](
            s"SELECT expand(out('TransactionBlock')) FROM Transaction WHERE transactionId='${transaction.id}'"
          )
            .map(_.asBlockBody)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def opens: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        resultSetQuery[OrientVertex](
          s"SELECT expand(out('TransactionBoxOpens')) FROM Transaction WHERE transactionId='${transaction.id}'"
        )
          .map(_.asBox)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def creates: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        resultSetQuery[OrientVertex](
          s"SELECT expand(out('TransactionBoxCreates')) FROM Transaction WHERE transactionId='${transaction.id}'"
        )
          .map(_.asBox)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def boxOps(box: Box): BoxOps =
    new BoxOps {

      override def createdBy: EitherT[Future, BlockchainOps.Error, Transaction] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT expand(in('TransactionBoxCreates')) FROM Box WHERE boxId='${box.id}'")
            .map(_.asTransaction)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def openedBy: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        resultSetQuery[OrientVertex](
          s"SELECT expand(in('TransactionBoxOpens')) FROM Box WHERE boxId='${box.id}'"
        )
          .map(_.asTransaction)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  private def applyCreateBlockHeader(
    createBlockHeader: CreateBlockHeader,
    state:             OrientDbBlockchainGraph.ModificationsState
  ) = {
    val header = createBlockHeader.header
    val v = session.addVertex("class:BlockHeader")
    v.setProperty("blockId", header.id)
    v.setProperty("timestamp", header.timestamp)
    v.setProperty("publicKey", header.publicKey)
    v.setProperty("signature", header.signature)
    v.setProperty("height", header.height)
    v.setProperty("difficulty", header.difficulty)
    v.setProperty("txRoot", header.txRoot)
    v.setProperty("bloomFilter", header.bloomFilter)
    v.setProperty("version", header.version)
    v.save()
    state.withVertex(v)
  }

  private def applyCreateBlockBody(
    createBlockBody: CreateBlockBody,
    state:           OrientDbBlockchainGraph.ModificationsState
  ) = {
    val body = createBlockBody.body
    val v = session.addVertex("class:BlockBody")
    v.setProperty("blockId", body.id)
    v.save()
    state.withVertex(v)
  }

  private def applyCreateTransaction(
    createTransaction: CreateTransaction,
    state:             OrientDbBlockchainGraph.ModificationsState
  ) = {
    val transaction = createTransaction.transaction

    val v = session.addVertex("class:Transaction")
    v.setProperty("transactionId", transaction.id)
    v.setProperty("fee", transaction.fee)
    v.setProperty("timestamp", transaction.timestamp)
    transaction.data.foreach(v.setProperty("data", _))
    v.setProperty("minting", transaction.minting)
    v.save()
    state.withVertex(v)
  }

  private def applyCreateBox(createBox: CreateBox, state: OrientDbBlockchainGraph.ModificationsState) = {
    val box = createBox.box
    val v = session.addVertex("class:Box")
    v.setProperty("boxId", box.id)
    v.setProperty("boxType", box.boxType)
    v.setProperty("evidence", box.evidence)
    v.setProperty("value", box.value)
    v.setProperty("nonce", box.nonce)
    v.save()
    state.withVertex(v)
  }

  private def applySetHead(setHead: SetHead, state: OrientDbBlockchainGraph.ModificationsState) = {
    var newHeadNode = false
    val head =
      session
        .command(new OCommandSQL("SELECT FROM Head WHERE name='HEAD'"))
        .execute[OrientDynaElementIterable]()
        .iterator()
        .asScala
        .to(LazyList)
        .headOption
        .map(_.asInstanceOf[OrientVertex])
        .getOrElse {
          newHeadNode = true
          val v = session.addVertex("class:Head")
          v.setProperty("name", "HEAD")
          v.save()
          v
        }

    val newTarget =
      state.newVertices
        .find(v => v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == setHead.blockId)
        .getOrElse(
          session
            .command(new OCommandSQL(s"SELECT FROM BlockHeader WHERE blockId='${setHead.blockId}'"))
            .execute[OrientDynaElementIterable]()
            .iterator()
            .next()
            .asInstanceOf[OrientVertex]
        )

    head.getEdges(Direction.OUT, "HeadBlock").iterator().asScala.foreach(_.remove())
    val e = head.addEdge("HeadBlock", newTarget)
    val withEdge =
      state.withEdge(e)

    if (newHeadNode) withEdge.withVertex(head) else withEdge
  }

  private def applyAssociateBlockToParent(
    associateBlockToParent: AssociateBlockToParent,
    state:                  OrientDbBlockchainGraph.ModificationsState
  ) = {
    val parentHeader = state.newVertices
      .find(v =>
        v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == associateBlockToParent.parentBlockId
      )
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM BlockHeader WHERE blockId='${associateBlockToParent.parentBlockId}'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )
    val childHeader = state.newVertices
      .find(v => v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == associateBlockToParent.childBlockId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM BlockHeader WHERE blockId='${associateBlockToParent.childBlockId}'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )
    val e = childHeader.addEdge("ParentBlock", parentHeader)

    state.withEdge(e)
  }

  private def applyAssociateBodyToHeader(
    associateBodyToHeader: AssociateBodyToHeader,
    state:                 OrientDbBlockchainGraph.ModificationsState
  ) = {
    val headerId = associateBodyToHeader.headerId
    val bodyId = associateBodyToHeader.bodyId
    val header = state.newVertices
      .find(v => v.getLabel == "BlockHeader" && v.getProperty[String]("blockId") == headerId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM BlockHeader WHERE blockId='$headerId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )
    val body = state.newVertices
      .find(v => v.getLabel == "BlockBody" && v.getProperty[String]("blockId") == bodyId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM BlockBody WHERE blockId='$bodyId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )

    val e = body.addEdge("BodyHeader", header)

    state.withEdge(e)
  }

  private def applyAssociateTransactionToBody(
    associateTransactionToBody: AssociateTransactionToBody,
    state:                      OrientDbBlockchainGraph.ModificationsState
  ) = {
    val transactionId = associateTransactionToBody.transactionId
    val blockId = associateTransactionToBody.blockId
    val transaction = state.newVertices
      .find(v => v.getLabel == "Transaction" && v.getProperty[String]("transactionId") == transactionId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM Transaction WHERE transactionId='$transactionId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )
    val body = state.newVertices
      .find(v => v.getLabel == "BlockBody" && v.getProperty[String]("blockId") == blockId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM BlockBody WHERE blockId='$blockId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )

    val e = transaction.addEdge("TransactionBlock", body)

    e.setProperty("index", associateTransactionToBody.index)

    state.withEdge(e)
  }

  private def applyAssociateBoxCreator(
    associateBoxCreator: AssociateBoxCreator,
    state:               OrientDbBlockchainGraph.ModificationsState
  ) = {

    val transactionId = associateBoxCreator.transactionId
    val boxId = associateBoxCreator.boxId

    val transaction = state.newVertices
      .find(v => v.getLabel == "Transaction" && v.getProperty[String]("transactionId") == transactionId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM Transaction WHERE transactionId='$transactionId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )

    val box = state.newVertices
      .find(v => v.getLabel == "Box" && v.getProperty[String]("boxId") == boxId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM Box WHERE boxId='$boxId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )

    val e = transaction.addEdge("TransactionBoxCreates", box)

    e.setProperty("minted", associateBoxCreator.minted)

    state.withEdge(e)
  }

  private def applyAssociateBoxOpener(
    associateBoxOpener: AssociateBoxOpener,
    state:              OrientDbBlockchainGraph.ModificationsState
  ) = {
    val transactionId = associateBoxOpener.transactionId
    val boxId = associateBoxOpener.boxId
    val transaction = state.newVertices
      .find(v => v.getLabel == "Transaction" && v.getProperty[String]("transactionId") == transactionId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM Transaction WHERE transactionId='$transactionId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )

    val box = state.newVertices
      .find(v => v.getLabel == "Box" && v.getProperty[String]("boxId") == boxId)
      .getOrElse(
        session
          .command(new OCommandSQL(s"SELECT FROM Box WHERE boxId='$boxId'"))
          .execute[OrientDynaElementIterable]()
          .iterator()
          .next()
          .asInstanceOf[OrientVertex]
      )

    val e = transaction.addEdge("TransactionBoxOpens", box)

    e.setProperty("attestation", associateBoxOpener.attestation)

    state.withEdge(e)
  }

  implicit override def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps = () =>
    EitherT(
      Source(modifications.toNonEmptyList.toList)
        .runFoldAsync(OrientDbBlockchainGraph.ModificationsState(Nil, Nil)) { case (state, modification) =>
          blockingOperation {
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
        }
        .map(_ => Right(Done))
        .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
    )

  implicit override def stringOps(value: String): StringOps =
    new StringOps {

      override def blockHeader: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT FROM BlockHeader WHERE blockId='$value'")
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def blockBody: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT FROM BlockBody WHERE blockId='$value'")
            .map(_.asBlockBody)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def transaction: EitherT[Future, BlockchainOps.Error, Transaction] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT FROM Transaction WHERE transactionId='$value'")
            .map(_.asTransaction)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def box: EitherT[Future, BlockchainOps.Error, Box] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT FROM Box WHERE boxId='$value'")
            .map(_.asBox)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )
    }

  implicit override def blockchainOps(blockchain: Blockchain.type): BlockchainOps =
    new BlockchainOps {

      override def currentHead: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT expand(out('HeadBlock')) FROM Head WHERE name='HEAD'")
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def currentHeads: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        resultSetQuery[OrientVertex](
          s"SELECT FROM BlockHeader WHERE in('ParentBlock').size()=0"
        )
          .map(_.asBlockHeader)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def genesis: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          resultSetQuery[OrientVertex](
            s"SELECT FROM BlockHeader WHERE out('ParentBlock').size()=0"
          )
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )
    }

  private def initialize(): Unit = {
    factory = location match {
      case OrientDbBlockchainGraph.InMemory =>
        new OrientGraphFactory(s"memory:blockchain")

      case OrientDbBlockchainGraph.Local(path) =>
        new OrientGraphFactory(s"plocal:$path")
    }
    session = factory.getNoTx

    initializeSchemas()

    system.registerOnTermination {
      session.shutdown()
      factory.close()
    }
  }

  private def blockingOperation[T](operation: => T): Future[T] =
    Future(operation)(blockingDispatcher)

  private def resultSetQuery[R](query: String): Source[R, NotUsed] =
    Source
      .fromIterator(() => session.command(new OCommandSQL(query)).execute[OrientDynaElementIterable]().asScala.iterator)
      .withAttributes(Attributes.name("OrientDBQuery").and(ActorAttributes.IODispatcher))
      .map(_.asInstanceOf[R])

  private def initializeSchemas(): Unit = {
    // TODO: Check for vertex/edge type existence
    val headerClass = session.createVertexType("BlockHeader")
    headerClass.createProperty("blockId", OType.STRING)
    headerClass.createProperty("timestamp", OType.LONG)
    headerClass.createProperty("publicKey", OType.STRING)
    headerClass.createProperty("height", OType.LONG)
    headerClass.createProperty("difficulty", OType.LONG)
    headerClass.createProperty("txRoot", OType.STRING)
    headerClass.createProperty("bloomFilter", OType.STRING)
    headerClass.createProperty("version", OType.BYTE)
    headerClass.createIndex("BlockHeader_idIndex", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX, "blockId")
    headerClass.createIndex("BlockHeader_timestampIndex", OClass.INDEX_TYPE.NOTUNIQUE, "timestamp")
    headerClass.createIndex("BlockHeader_heightIndex", OClass.INDEX_TYPE.NOTUNIQUE, "height")
    headerClass.createIndex("BlockHeader_bloomFilter", OClass.INDEX_TYPE.NOTUNIQUE, "bloomFilter")

    val bodyClass = session.createVertexType("BlockBody")
    bodyClass.createProperty("blockId", OType.STRING)
    bodyClass.createIndex("BlockBody_idIndex", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX, "blockId")

    val transactionClass = session.createVertexType("Transaction")
    transactionClass.createProperty("transactionId", OType.STRING)
    transactionClass.createProperty("fee", OType.STRING)
    transactionClass.createProperty("timestamp", OType.LONG)
    transactionClass.createProperty("data", OType.STRING)
    transactionClass.createProperty("minting", OType.BOOLEAN)
    transactionClass.createIndex("Transaction_idIndex", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX, "transactionId")
    transactionClass.createIndex("Transaction_timestampIndex", OClass.INDEX_TYPE.NOTUNIQUE, "timestamp")

    val boxClass = session.createVertexType("Box")
    boxClass.createProperty("boxId", OType.STRING)
    boxClass.createProperty("boxType", OType.STRING)
    boxClass.createProperty("evidence", OType.STRING)
    boxClass.createProperty("value", OType.STRING)
    boxClass.createProperty("nonce", OType.LONG)
    boxClass.createIndex("Box_idIndex", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX, "boxId")

    val headClass = session.createVertexType("Head")
    headClass.createProperty("name", OType.STRING)
    headClass.createIndex("Head_nameIndex", OClass.INDEX_TYPE.UNIQUE_HASH_INDEX, "name")

    val parentBlockEdgeClass = session.createEdgeType("ParentBlock")
    headerClass.createEdgeProperty(Direction.IN, "ParentBlock")
    headerClass.createEdgeProperty(Direction.OUT, "ParentBlock")
    val bodyHeaderEdgeClass = session.createEdgeType("BodyHeader")
    headerClass.createEdgeProperty(Direction.IN, "BodyHeader")
    bodyClass.createEdgeProperty(Direction.OUT, "BodyHeader")
    val transactionBlockEdgeClass = session.createEdgeType("TransactionBlock")
    transactionBlockEdgeClass.createProperty("index", OType.INTEGER)
    bodyClass.createEdgeProperty(Direction.IN, "TransactionBlock")
    transactionClass.createEdgeProperty(Direction.OUT, "TransactionBlock")
    val transactionBoxCreatesEdgeClass = session.createEdgeType("TransactionBoxCreates")
    transactionBoxCreatesEdgeClass.createProperty("minted", OType.BOOLEAN)
    transactionClass.createEdgeProperty(Direction.OUT, "TransactionBoxCreates")
    boxClass.createEdgeProperty(Direction.OUT, "TransactionBoxCreates")
    val transactionBoxOpensEdgeClass = session.createEdgeType("TransactionBoxOpens")
    transactionBoxOpensEdgeClass.createProperty("attestation", OType.STRING)
    transactionClass.createEdgeProperty(Direction.OUT, "TransactionBoxOpens")
    boxClass.createEdgeProperty(Direction.OUT, "TransactionBoxOpens")

    headClass.createEdgeProperty(Direction.OUT, "HeadBlock")
  }
}

object OrientDbBlockchainGraph {

  private case class ModificationsState(newVertices: List[OrientVertex], newEdges: List[Edge]) {
    def withVertex(v: OrientVertex): ModificationsState = copy(newVertices :+ v)
    def withEdge(e:   Edge): ModificationsState = copy(newEdges = newEdges :+ e)
  }

  sealed abstract class Location
  case object InMemory extends Location
  case class Local(path: Path) extends Location

  trait Implicits {

    implicit class OrientVertexOps(vertex: Vertex) {

      def asBlockHeader: BlockHeader = BlockHeader(
        vertex.getProperty("blockId"),
        vertex.getProperty("timestamp"),
        vertex.getProperty("publicKey"),
        vertex.getProperty("signature"),
        vertex.getProperty("height"),
        vertex.getProperty("difficulty"),
        vertex.getProperty("txRoot"),
        vertex.getProperty("bloomFilter"),
        vertex.getProperty("version")
      )

      def asBlockBody: BlockBody = BlockBody(
        vertex.getProperty("blockId")
      )

      def asTransaction: Transaction = Transaction(
        vertex.getProperty("transactionId"),
        vertex.getProperty("fee"),
        vertex.getProperty("timestamp"),
        Option(vertex.getProperty("data")),
        vertex.getProperty("minting")
      )

      def asBox: Box = Box(
        vertex.getProperty("boxId"),
        vertex.getProperty("boxType"),
        vertex.getProperty("evidence"),
        vertex.getProperty("value"),
        vertex.getProperty("nonce")
      )
    }
  }
  object implicits extends Implicits
}
