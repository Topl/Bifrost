package co.topl.storage.graph

import akka.actor.typed.scaladsl.AskPattern._
import akka.actor.typed.scaladsl.Behaviors
import akka.actor.typed._
import akka.pattern.StatusReply
import akka.stream.scaladsl.{Sink, Source}
import akka.util.Timeout
import akka.{Done, NotUsed}
import cats.data.EitherT
import cats.implicits._
import com.orientechnologies.orient.core.config.OGlobalConfiguration
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient._
import com.tinkerpop.blueprints.{Direction, Edge}

import java.nio.file.Path
import scala.concurrent.duration._
import scala.concurrent.{Await, ExecutionContext, Future}
import scala.jdk.CollectionConverters._
import scala.util.{Failure, Success, Try}

class OrientDBGraph(schema: GraphSchema, factory: OrientGraphFactory)(implicit system: ActorSystem[_])
    extends ReadableGraph {

  import OrientDBGraph._
  import Ops._
  import system.executionContext

  private val workDispatcher =
    system.systemActorOf(OrientDBGraphActorParent(factory), "graph-work-dispatcher")

  implicit private val timeout: Timeout =
    10.hours

  initialize()

  def transactionally[Result](
    f: ReadableWritableGraph => Future[Result]
  ): Future[Result] =
    workDispatcher.askWithStatus[Result](ref =>
      OrientDBGraphActorParent.TxWork(
        OrientDbTransactionActor.Execute(
          (session, blockingExecutionContext) =>
            f(
              new ReadableWritableGraph(
                new ReadableOrientGraph(session, blockingExecutionContext),
                new WritableOrientGraph(session, blockingExecutionContext)
              )
            ),
          ref
        )
      )
    )

  def getNode[T: NodeSchema](graphQuery: GraphQuery[T]): EitherT[Future, OrientDBGraph.Error, Option[T]] =
    EitherT(
      workDispatcher
        .askWithStatus[Option[T]](ref =>
          OrientDBGraphActorParent.Work(
            OrientDBGraphActor.Execute(
              session =>
                session
                  .command(new OCommandSQL(stringifyQuery(graphQuery) + " LIMIT 1"))
                  .execute[OrientDynaElementIterable]()
                  .iterator()
                  .asScala
                  .collect { case r: OrientVertex @unchecked => r.as[T] }
                  .nextOption(),
              ref
            )
          )
        )
        .map(Right(_))
        .recover { case e => Left(OrientDBGraph.ThrowableError(e)) }
    )

  def getNodes[T: NodeSchema](graphQuery: GraphQuery[T]): Source[Either[OrientDBGraph.Error, T], NotUsed] = {
    val query = stringifyQuery(graphQuery)

    Source
      .futureSource(
        workDispatcher.ask[Source[Either[OrientDBGraph.Error, T], NotUsed]](ref =>
          OrientDBGraphActorParent.Work(
            OrientDBGraphActor.ExecuteIterator(
              session =>
                session
                  .command(new OCommandSQL(query))
                  .execute[OrientDynaElementIterable]()
                  .iterator()
                  .asScala
                  .collect { case r: OrientVertex @unchecked => r.as[T].asRight },
              ref
            )
          )
        )
      )
      .mapMaterializedValue(_ => NotUsed)
  }

  private def initialize(): Unit =
    Await.result(initializeSchemas(), 30.seconds)

  def close(): Unit =
    factory.close()

  private def initializeSchemas(): Future[Done] = Future {
    val session = factory.getNoTx
    schema.edgeSchemas.foreach { edgeSchema =>
      Option(session.getEdgeType(edgeSchema.name)) match {
        case Some(_) =>
        case None =>
          val edgeType = session.createEdgeType(edgeSchema.name)
          edgeSchema.properties.foreach(property => edgeType.createProperty(property.name, property.propertyType))
          edgeSchema.indices.foreach(index => edgeType.createIndex(index.name, index.indexType, index.propertyName))
      }
    }

    schema.nodeSchemas.foreach { nodeSchema =>
      Option(session.getVertexType(nodeSchema.name)) match {
        case Some(_) =>
        case None =>
          val vertexType = session.createVertexType(nodeSchema.name)
          nodeSchema.properties.foreach(property => vertexType.createProperty(property.name, property.propertyType))
          nodeSchema.indices.foreach(index => vertexType.createIndex(index.name, index.indexType, index.propertyName))
      }
    }
    schema.edgeSchemas
      .foreach { edgeSchema =>
        val srcVertex = session.getVertexType(edgeSchema.srcSchema.name)
        val destVertex = session.getVertexType(edgeSchema.destSchema.name)
        srcVertex.createEdgeProperty(Direction.OUT, edgeSchema.name)
        destVertex.createEdgeProperty(Direction.IN, edgeSchema.name)
      }
    session.shutdown()
    Done
  }

  def dump(): String = {
    val builder = new StringBuilder
    val session = factory.getTx
    session.getVertices.asScala.foreach { v =>
      builder.append(v)
      builder.append(v.getPropertyKeys.asScala.map(key => key -> v.getProperty(key)).toMap)
      builder.append('\n')
    }
    session.getEdges.asScala.foreach { e =>
      builder.append(e)
      builder.append(e.getPropertyKeys.asScala.map(key => key -> e.getProperty(key)).toMap)
      builder.append('\n')
    }
    session.shutdown()
    builder.result()
  }

}

trait ReadableGraph {
  def getNode[T: NodeSchema](graphQuery:  GraphQuery[T]): EitherT[Future, OrientDBGraph.Error, Option[T]]
  def getNodes[T: NodeSchema](graphQuery: GraphQuery[T]): Source[Either[OrientDBGraph.Error, T], NotUsed]
}

trait WritableGraph {
  def insertNode[T: NodeSchema](node: T): EitherT[Future, OrientDBGraph.Error, Done]

  def insertEdge[T](edge: T, srcRef: OrientDBGraph.NodeReference, destRef: OrientDBGraph.NodeReference)(implicit
    schema:               EdgeSchema[T, _, _]
  ): EitherT[Future, OrientDBGraph.Error, Done]

  def deleteEdges[T]()(implicit
    schema: EdgeSchema[T, _, _]
  ): EitherT[Future, OrientDBGraph.Error, Done]
}

object OrientDBGraph {

  sealed trait Error
  case class ThrowableError(throwable: Throwable) extends Error

  sealed abstract class Location
  case object InMemory extends Location
  case class Local(path: Path) extends Location

  def apply(schema: GraphSchema, location: Location)(implicit system: ActorSystem[_]): OrientDBGraph = {
    OGlobalConfiguration.RID_BAG_EMBEDDED_TO_SBTREEBONSAI_THRESHOLD.setValue(-1)

    val factory = location match {
      case InMemory =>
        new OrientGraphFactory(s"memory:blockchain", true)

      case Local(path) =>
        new OrientGraphFactory(s"plocal:$path", true)
    }
    new OrientDBGraph(schema, factory)
  }

  case class NodeReference(query: GraphQuery[_])

  private[graph] def whereToString(where: Where): Option[String] =
    where match {
      case WhereAny => None
      case PropEquals(name, value) =>
        val value1 = value match {
          case s1: String => "\"" + s1 + "\""
          case s1         => s1.toString
        }
        Some(s"$name=$value1")
      case And(op1, op2) =>
        (whereToString(op1), whereToString(op2)) match {
          case (Some(l), Some(r)) => Some(s"$l && $r")
          case (Some(l), _)       => Some(l)
          case (_, Some(r))       => Some(r)
          case _                  => None
        }
      case Or(op1, op2) =>
        (whereToString(op1), whereToString(op2)) match {
          case (Some(l), Some(r)) => Some(s"$l || $r")
          case (Some(l), _)       => Some(l)
          case (_, Some(r))       => Some(r)
          case _                  => None
        }
    }

  private[graph] def stringifyQuery[T](graphQuery: GraphQuery[T]): String =
    graphQuery match {
      case q @ NodesByClass(where) =>
        "SELECT" +
          s" FROM ${q.resultSchema.name}" +
          whereToString(where).fold("")(" WHERE " + _)
      case f @ Trace(where, edges) =>
        val expansion = edges
          .map { edgeWithDirection =>
            val directionString =
              edgeWithDirection.direction match {
                case In  => "in"
                case Out => "out"
              }
            s"$directionString('${edgeWithDirection.edgeSchema.name}')"
          }
          .mkString(".")
        s"SELECT expand($expansion)" +
        s" FROM ${f.originNodeSchema.name}" +
        whereToString(where).fold("")(" WHERE " + _)
      case Traverse(origin, edges) =>
        val expansion = edges
          .map { edgeWithDirection =>
            val directionString =
              edgeWithDirection.direction match {
                case In  => "in"
                case Out => "out"
              }
            s"$directionString('${edgeWithDirection.edgeSchema.name}')"
          }
          .mkString(".")
        val originQuery = stringifyQuery(origin)
        s"TRAVERSE $expansion" +
        s" FROM ($originQuery)"
      case Raw(query) =>
        query
    }

  object Ops {

    implicit class VertexOps(orientVertex: OrientVertex) {

      def as[T: NodeSchema]: T =
        implicitly[NodeSchema[T]].decode(orientVertex.getProperties.asScala.toMap + ("@class" -> orientVertex.getLabel))
    }

    implicit class EdgeOps(edge: Edge) {

      def as[T](implicit schema: EdgeSchema[T, _, _]): T =
        schema.decode(
          edge.getPropertyKeys.asScala
            .map(key => key -> edge.getProperty(key))
            .toMap + ("@class" -> edge.getLabel)
        )
    }
  }
}

private object OrientDBGraphActorParent {

  def apply(factory: OrientGraphFactory): Behavior[Message] = {
    def withState(
      readyForWork:   List[ActorRef[OrientDBGraphActor.Message[_]]],
      readyForTxWork: List[ActorRef[OrientDbTransactionActor.Message]]
    ): Behavior[Message] =
      Behaviors.receive((ctx, message) =>
        message match {
          case GiveWork(to) =>
            withState(readyForWork :+ to, readyForTxWork)
          case Work(message) =>
            val (worker, newReadyForWork) =
              readyForWork match {
                case Nil =>
                  val w =
                    ctx.spawnAnonymous(
                      OrientDBGraphActor(() => factory.getNoTx, ctx.self),
                      DispatcherSelector.fromConfig("orient-db-actor-dispatcher")
                    )
                  (w, readyForWork)
                case w :: rest =>
                  (w, rest)
              }

            worker ! message

            withState(newReadyForWork, readyForTxWork)

          case GiveTxWork(to) =>
            withState(readyForWork, readyForTxWork :+ to)

          case TxWork(message) =>
            val (txWorker, newReadyForTxWork) =
              readyForTxWork match {
                case Nil =>
                  val w =
                    ctx.spawnAnonymous(
                      OrientDbTransactionActor(factory, ctx.self),
                      DispatcherSelector.fromConfig("orient-db-actor-dispatcher")
                    )
                  (w, readyForTxWork)
                case w :: rest =>
                  (w, rest)
              }

            txWorker ! message

            withState(readyForWork, newReadyForTxWork)
        }
      )
    withState(Nil, Nil)
  }

  sealed abstract class Message
  case class GiveWork(to: ActorRef[OrientDBGraphActor.Message[_]]) extends Message
  case class GiveTxWork(to: ActorRef[OrientDbTransactionActor.Message]) extends Message
  case class Work[Response](message: OrientDBGraphActor.Message[Response]) extends Message
  case class TxWork[Response](message: OrientDbTransactionActor.Message) extends Message
}

private object OrientDBGraphActor {

  def apply(factory: () => OrientBaseGraph, parent: ActorRef[OrientDBGraphActorParent.Message]): Behavior[Message[_]] =
    Behaviors.setup { ctx =>
      import ctx.executionContext
      def idle: Behavior[Message[_]] =
        Behaviors.receiveMessagePartial {
          case e: Execute[_] =>
            val session = factory()
            ctx.pipeToSelf(e.runAndReply(session, ctx.executionContext))(CompleteExecution(_))
            withSession(session)
          case e: ExecuteIterator[_] =>
            val session = factory()
            e.runAndReply(session, ctx.self, ctx.executionContext)
            Behaviors.same
            withSession(session)
        }

      def withSession(session: OrientBaseGraph): Behavior[Message[_]] =
        Behaviors
          .receiveMessagePartial[Message[_]] { case _: CompleteExecution[_] =>
            parent.tell(OrientDBGraphActorParent.GiveWork(ctx.self))
            session.shutdown()
            idle
          }
          .receiveSignal { case (_, PostStop) =>
            session.shutdown()
            Behaviors.same
          }

      idle
    }

  sealed abstract class Message[+Response]

  case class Execute[T](f: OrientBaseGraph => T, replyTo: ActorRef[StatusReply[T]]) extends Message {

    def runAndReply(session: OrientBaseGraph, blockingEc: ExecutionContext)(implicit ec: ExecutionContext): Future[T] =
      Future(f(session))(blockingEc)
        .andThen {
          case Success(v)         => replyTo.tell(StatusReply.Success(v))
          case Failure(exception) => replyTo ! StatusReply.Error(exception)
        }
  }

  case class ExecuteIterator[T](f: OrientBaseGraph => Iterator[T], replyTo: ActorRef[Source[T, NotUsed]])
      extends Message {

    def runAndReply(
      session:                  OrientBaseGraph,
      worker:                   ActorRef[OrientDBGraphActor.Message[_]],
      blockingExecutionContext: ExecutionContext
    ): Unit = {
      val source =
        iteratorSourceOnDispatcher(() => f(session), blockingExecutionContext)
          .alsoTo(Sink.onComplete(result => worker ! CompleteExecution(result)))
      replyTo.tell(source)
    }
  }

  case class CompleteExecution[T](result: Try[T]) extends Message

}

private object OrientDbTransactionActor {

  def apply(factory: OrientGraphFactory, parent: ActorRef[OrientDBGraphActorParent.Message]): Behavior[Message] =
    Behaviors.setup { ctx =>
      val session = factory.getTx
      session.setAutoStartTx(false)

      Behaviors
        .receiveMessage[Message] {
          case Execute(f, replyTo) =>
            session.begin()
            ctx.pipeToSelf(f(session, ctx.executionContext))(CompleteExecution(_, replyTo))
            Behaviors.same
          case CompleteExecution(result, replyTo) =>
            result.flatMap(r => Try(session.commit()).map(_ => r)) match {
              case Failure(exception) =>
                session.rollback()
                replyTo ! StatusReply.Error(exception)
              case Success(value) =>
                replyTo ! StatusReply.Success(value)
            }
            parent.tell(OrientDBGraphActorParent.GiveTxWork(ctx.self))
            Behaviors.same
        }
        .receiveSignal { case (_, PostStop) =>
          session.shutdown()
          Behaviors.stopped[Message]
        }
    }

  sealed abstract class Message

  case class Execute[T](f: (OrientBaseGraph, ExecutionContext) => Future[T], replyTo: ActorRef[StatusReply[T]])
      extends Message
  case class CompleteExecution[T](result: Try[T], replyTo: ActorRef[StatusReply[T]]) extends Message
}

abstract class OrientGraphBaseScala(orientGraph: OrientBaseGraph, blockingDispatcher: ExecutionContext) {

  import OrientDBGraph._

  def runRawCommand[Result](raw: String): Future[Result] = blockingOperation {
    orientGraph
      .command(new OCommandSQL(raw))
      .execute[Result]()
  }

  protected def blockingOperation[T](operation: => T): Future[T] =
    Future {
      operation
    }(blockingDispatcher)

  protected def blockingIteratorQuery(query: String): Iterator[OrientElement] =
    orientGraph
      .command(new OCommandSQL(query))
      .execute[OrientDynaElementIterable]()
      .iterator()
      .asScala
      .map(_.asInstanceOf[OrientElement])

  protected def resolveNodeReference(ref: OrientDBGraph.NodeReference): OrientVertex =
    blockingIteratorQuery(stringifyQuery(ref.query)).next().asInstanceOf[OrientVertex]
}

class ReadableOrientGraph(orientGraph: OrientBaseGraph, blockingDispatcher: ExecutionContext)
    extends OrientGraphBaseScala(orientGraph, blockingDispatcher)
    with ReadableGraph {

  import OrientDBGraph._
  import Ops._

  override def getNode[T: NodeSchema](graphQuery: GraphQuery[T]): EitherT[Future, OrientDBGraph.Error, Option[T]] =
    EitherT(
      blockingOperation {
        blockingIteratorQuery(stringifyQuery(graphQuery))
          .collect { case r: OrientVertex @unchecked => r.as[T] }
          .nextOption()
          .asRight
      }
    )

  override def getNodes[T: NodeSchema](graphQuery: GraphQuery[T]): Source[Either[OrientDBGraph.Error, T], NotUsed] =
    iteratorSourceOnDispatcher(
      () =>
        blockingIteratorQuery(stringifyQuery(graphQuery))
          .collect { case r: OrientVertex @unchecked => r.as[T].asRight },
      blockingDispatcher
    )

}

class WritableOrientGraph(orientGraph: OrientBaseGraph, blockingDispatcher: ExecutionContext)
    extends OrientGraphBaseScala(orientGraph, blockingDispatcher)
    with WritableGraph {

  implicit val ec: ExecutionContext = blockingDispatcher
  import OrientDBGraph._

  override def insertNode[T: NodeSchema](node: T): EitherT[Future, OrientDBGraph.Error, Done] =
    EitherT(
      blockingOperation {
        val schema = implicitly[NodeSchema[T]]
        val v = orientGraph.addVertex(s"class:${schema.name}")
        schema.encode(node).foreach { case (name, value) =>
          v.setProperty(name, value)
        }
        v.save()
        Right(Done)
      }
        .recover { case e => Left(OrientDBGraph.ThrowableError(e)) }
    )

  override def insertEdge[T](edge: T, srcRef: OrientDBGraph.NodeReference, destRef: OrientDBGraph.NodeReference)(
    implicit schema:               EdgeSchema[T, _, _]
  ): EitherT[Future, OrientDBGraph.Error, Done] =
    EitherT(
      runRawCommand[Any](
        s"CREATE EDGE ${schema.name}" +
        s" FROM (${stringifyQuery(srcRef.query)} LIMIT 1)" +
        s" TO (${stringifyQuery(destRef.query)} LIMIT 1)"
      )
        .map(_ => Right(Done))
        .recover { case e => Left(OrientDBGraph.ThrowableError(e)) }
    )

  override def deleteEdges[T]()(implicit
    schema: EdgeSchema[T, _, _]
  ): EitherT[Future, OrientDBGraph.Error, Done] =
    EitherT(
      blockingOperation {
        orientGraph
          .getEdgesOfClass(schema.name)
          .iterator()
          .asScala
          .foreach(edge => edge.remove())
        Right(Done)
      }
        .recover { case e => Left(OrientDBGraph.ThrowableError(e)) }
    )
}

class ReadableWritableGraph(readableGraph: ReadableGraph, writableGraph: WritableGraph)
    extends ReadableGraph
    with WritableGraph {

  override def getNode[T: NodeSchema](graphQuery: GraphQuery[T]): EitherT[Future, OrientDBGraph.Error, Option[T]] =
    readableGraph.getNode(graphQuery)

  override def getNodes[T: NodeSchema](graphQuery: GraphQuery[T]): Source[Either[OrientDBGraph.Error, T], NotUsed] =
    readableGraph.getNodes(graphQuery)

  override def insertNode[T: NodeSchema](node: T): EitherT[Future, OrientDBGraph.Error, Done] =
    writableGraph.insertNode(node)

  override def insertEdge[T](edge: T, srcRef: OrientDBGraph.NodeReference, destRef: OrientDBGraph.NodeReference)(
    implicit schema:               EdgeSchema[T, _, _]
  ): EitherT[Future, OrientDBGraph.Error, Done] = writableGraph.insertEdge(edge, srcRef, destRef)

  override def deleteEdges[T]()(implicit schema: EdgeSchema[T, _, _]): EitherT[Future, OrientDBGraph.Error, Done] =
    writableGraph.deleteEdges()
}
