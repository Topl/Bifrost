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
import com.tinkerpop.blueprints.{Edge, Vertex}

import java.nio.file.Path
import scala.concurrent.Future
import scala.jdk.CollectionConverters._
import scala.language.implicitConversions

class OrientDbBlockchainGraph(path: Path)(implicit system: ActorSystem) extends BlockchainOpsProvider {

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
          resultSetQuery[OrientVertex](s"SELECT expand(out('ParentBlock')) FROM BlockHeader WHERE id='${header.id}'")
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def childBlocks: Source[Either[BlockchainOps.Error, BlockHeader], NotUsed] =
        resultSetQuery[OrientVertex](s"SELECT expand(in('ParentBlock')) FROM BlockHeader WHERE id='${header.id}'")
          .map(_.asBlockHeader)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def body: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT expand(in('BodyHeader')) FROM BlockHeader WHERE id='${header.id}'")
            .map(_.asBlockBody)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )
    }

  implicit override def bodyOps(body: BlockBody): BlockBodyOps =
    new BlockBodyOps {

      override def header: EitherT[Future, BlockchainOps.Error, BlockHeader] =
        EitherT(
          resultSetQuery[OrientVertex](s"SELECT expand(out('BodyHeader')) FROM BlockBody WHERE id='${body.id}'")
            .map(_.asBlockHeader)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def transactions: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        resultSetQuery[OrientVertex](s"SELECT expand(in('TransactionBlock')) FROM BlockBody WHERE id='${body.id}'")
          .map(_.asTransaction)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def transactionOps(transaction: Transaction): TransactionOps =
    new TransactionOps {

      override def body: EitherT[Future, BlockchainOps.Error, BlockBody] =
        EitherT(
          resultSetQuery[OrientVertex](
            s"SELECT expand(out('TransactionBlock')) FROM Transaction WHERE id='${transaction.id}'"
          )
            .map(_.asBlockBody)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def opens: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        resultSetQuery[OrientVertex](
          s"SELECT expand(out('TransactionBoxOpens')) FROM Transaction WHERE id='${transaction.id}'"
        )
          .map(_.asBox)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)

      override def creates: Source[Either[BlockchainOps.Error, Box], NotUsed] =
        resultSetQuery[OrientVertex](
          s"SELECT expand(out('TransactionBoxCreates')) FROM Transaction WHERE id='${transaction.id}'"
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
          resultSetQuery[OrientVertex](s"SELECT expand(in('TransactionBoxCreates')) FROM Box WHERE id='${box.id}'")
            .map(_.asTransaction)
            .runWith(Sink.head)
            .map(Right(_))
            .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
        )

      override def openedBy: Source[Either[BlockchainOps.Error, Transaction], NotUsed] =
        resultSetQuery[OrientVertex](s"SELECT expand(in('TransactionBoxOpens')) FROM Transaction WHERE id='${box.id}'")
          .map(_.asTransaction)
          .map(Right(_))
          .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
          .takeWhile(_.isRight, inclusive = true)
    }

  implicit override def blockchainModificationsOps(
    modifications: NonEmptyChain[BlockchainModification]
  ): BlockchainModificationsOps = () =>
    EitherT(
      Source(modifications.toNonEmptyList.toList)
        .runFoldAsync(OrientDbBlockchainGraph.ModificationsState(Nil, Nil)) { case (state, modification) =>
          blockingOperation {
            modification match {
              case CreateBlockHeader(header) =>
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
              case CreateBlockBody(body) =>
                val v = session.addVertex("class:BlockHeader")
                v.setProperty("blockId", body.id)
                v.save()
                state.withVertex(v)
              case CreateTransaction(transaction) =>
                val v = session.addVertex("class:Transaction")
                v.setProperty("transactionId", transaction.id)
                v.setProperty("fee", transaction.fee)
                v.setProperty("timestamp", transaction.timestamp)
                transaction.data.foreach(v.setProperty("data", _))
                v.setProperty("minting", transaction.minting)
                v.save()
                state.withVertex(v)
              case CreateBox(box) =>
                val v = session.addVertex("class:Box")
                v.setProperty("boxId", box.id)
                v.setProperty("boxType", box.boxType)
                v.setProperty("evidence", box.evidence)
                v.setProperty("value", box.value)
                v.setProperty("nonce", box.nonce)
                v.save()
                state.withVertex(v)
              case AssociateBodyToHeader(headerId, bodyId) =>
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
              case AssociateTransactionToBody(transactionId, blockId, index) =>
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

                val e = transaction.addEdge("TransactionBody", body)

                e.setProperty("index", index)

                state.withEdge(e)
              case AssociateBoxCreator(boxId, transactionId, minted) =>
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

                e.setProperty("minted", minted)

                state.withEdge(e)

              case AssociateBoxOpener(boxId, transactionId, attestation) =>
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

                e.setProperty("attestation", attestation)

                state.withEdge(e)
            }
          }
        }
        .map(_ => Right(Done))
        .recover { case e => Left(BlockchainOps.ThrowableError(e)) }
    )

  private def initialize(): Unit = {
    factory = new OrientGraphFactory(s"plocal:$path")
    session = factory.getNoTx

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
    transactionClass.createProperty("timestamp", OType.DATETIME)
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

    val parentBlockEdgeClass = session.createEdgeType("ParentBlock")
    val bodyHeaderEdgeClass = session.createEdgeType("BodyHeader")
    val transactionBlockEdgeClass = session.createEdgeType("TransactionBlock")
    transactionBlockEdgeClass.createProperty("index", OType.INTEGER)
    val transactionBoxCreatesEdgeClass = session.createEdgeType("TransactionBoxCreates")
    transactionBoxCreatesEdgeClass.createProperty("minted", OType.BOOLEAN)
    val transactionBoxOpensEdgeClass = session.createEdgeType("TransactionBoxOpens")
    transactionBoxOpensEdgeClass.createProperty("attestation", OType.STRING)
  }

  private def blockingOperation[T](operation: => T): Future[T] =
    Future(operation)(blockingDispatcher)

  private def resultSetQuery[R](query: String): Source[R, NotUsed] =
    Source
      .fromIterator(() => session.command(new OCommandSQL(query)).execute[OrientDynaElementIterable]().asScala.iterator)
      .withAttributes(Attributes.name("OrientDBQuery").and(ActorAttributes.IODispatcher))
      .map(_.asInstanceOf[R])

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

  system.registerOnTermination {
    session.shutdown()
    factory.close()
  }
}

object OrientDbBlockchainGraph {

  private case class ModificationsState(newVertices: List[OrientVertex], newEdges: List[Edge]) {
    def withVertex(v: OrientVertex): ModificationsState = copy(newVertices :+ v)
    def withEdge(e:   Edge): ModificationsState = copy(newEdges = newEdges :+ e)
  }

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
