package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorAttributes, Attributes}
import akka.{Done, NotUsed}
import cats.data.EitherT
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient._
import com.tinkerpop.blueprints.{Direction, Edge}
import cats.implicits._

import java.nio.file.Path
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class OrientDBGraph(schema: GraphSchema, factory: OrientGraphFactory)(implicit system: ActorSystem) {

  import system.dispatcher

  private val blockingDispatcher = system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)
  var session: OrientGraphNoTx = _

  initialize()

  private def initialize(): Unit = {
    session = factory.getNoTx
    initializeSchemas()

    system.registerOnTermination {
      session.shutdown()
      factory.close()
    }
  }

  private def blockingIteratorQuery(query: String): Iterator[OrientElement] =
    session
      .command(new OCommandSQL(query))
      .execute[OrientDynaElementIterable]()
      .iterator()
      .asScala
      .map(_.asInstanceOf[OrientElement])

  def insertNode[T: NodeSchema](node: T): Future[OrientVertex] =
    blockingOperation {
      val schema = implicitly[NodeSchema[T]]
      val v = session.addVertex(s"class:${schema.name}")
      schema.encode(node).foreach { case (name, value) =>
        v.setProperty(name, value)
      }
      v.save()
      v
    }

  private def resolveNodeReference(ref: OrientDBGraph.NodeReference): OrientVertex =
    ref match {
      case OrientDBGraph.QueryNodeReference(query) =>
        blockingIteratorQuery(queryString(query)).next().asInstanceOf[OrientVertex]
      case OrientDBGraph.VertexNodeReference(orientVertex) =>
        orientVertex
    }

  def insertEdge[T](edge: T, srcRef: OrientDBGraph.NodeReference, destRef: OrientDBGraph.NodeReference)(implicit
    schema:               EdgeSchema[T, _, _]
  ): Future[Edge] =
    blockingOperation {
      val src = resolveNodeReference(srcRef)
      val dest = resolveNodeReference(destRef)
      val e = src.addEdge(schema.name, dest)
      schema.encode(edge).foreach { case (name, value) =>
        e.setProperty(name, value)
      }
      e
    }

  private def initializeSchemas(): Unit = {
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
  }

  private def blockingOperation[T](operation: => T): Future[T] =
    Future {
      operation
    }(blockingDispatcher)

  private def blockingIteratorSource[T](iteratorFactory: () => Iterator[T]): Source[T, NotUsed] =
    Source
      .fromIterator(iteratorFactory)
      .withAttributes(Attributes.name("OrientDBQuery").and(ActorAttributes.IODispatcher))

  private def resultSetQuery[R <: OrientElement](query: String): Source[Either[OrientDBGraph.Error, R], NotUsed] =
    blockingIteratorSource(() =>
      session
        .command(new OCommandSQL(query))
        .execute[OrientDynaElementIterable]()
        .iterator()
        .asScala
        .collect { case r: R @unchecked => r.asRight }
    )

//  def getNode[T: NodeSchema](query: String): Future[Option[T]] = {
//    import Ops._
//    resultSetQuery[OrientVertex](query)
//      .map(_.as[T])
//      .runWith(Sink.headOption)
//  }

  private def whereToString(where: Where): Option[String] =
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

  def getNode[T: NodeSchema](graphQuery: GraphQuery[T]): EitherT[Future, OrientDBGraph.Error, Option[T]] =
    EitherT(
      getNodes(graphQuery)
        .runWith(Sink.headOption)
        .map {
          case Some(Right(value)) => Right(Some(value))
          case Some(Left(e))      => Left(e)
          case None               => Right(None)
        }
    )

  def getRawNode(graphQuery: GraphQuery[_]): EitherT[Future, OrientDBGraph.Error, Option[OrientVertex]] =
    EitherT(
      resultSetQuery[OrientVertex](queryString(graphQuery))
        .runWith(Sink.headOption)
        .map {
          case Some(Right(value)) => Right(Some(value))
          case Some(Left(e))      => Left(e)
          case None               => Right(None)
        }
    )

  private def queryString[T](graphQuery: GraphQuery[T]): String =
    graphQuery match {
      case q @ NodesByClass(where) =>
        "SELECT" +
          s" FROM ${q.resultSchema.name}" +
          whereToString(where).fold("")(" WHERE " + _)
      case f @ Trace(where, edges) =>
        val expansion = edges
          .map { edgeWithDirection =>
            val directionString = {
              edgeWithDirection.direction match {
                case In  => "in"
                case Out => "out"
              }
            }
            s"$directionString('${edgeWithDirection.edgeSchema.name}')"
          }
          .mkString(".")
        s"SELECT expand($expansion)" +
        s" FROM ${f.originNodeSchema.name}" +
        whereToString(where).fold("")(" WHERE " + _)
      case t @ Traverse(origin, edges) =>
        //          s"TRAVERSE out('${BlockParent.edgeSchema.name}')" +
        //            s" FROM (SELECT FROM ${BlockHeader.nodeSchema.name}" +
        //            s" WHERE blockId='${header.blockId}')"
        val expansion = edges
          .map { edgeWithDirection =>
            val directionString = {
              edgeWithDirection.direction match {
                case In  => "in"
                case Out => "out"
              }
            }
            s"$directionString('${edgeWithDirection.edgeSchema.name}')"
          }
          .mkString(".")
        val originQuery = queryString(origin)
        s"TRAVERSE $expansion" +
        s" FROM ($originQuery)"
    }

  def getNodes[T: NodeSchema](graphQuery: GraphQuery[T]): Source[Either[OrientDBGraph.Error, T], NotUsed] = {
    import Ops._
    resultSetQuery[OrientVertex](queryString(graphQuery))
      .map(_.map(_.as[T]))
  }

  object Ops {

    implicit class VertexOps(orientVertex: OrientVertex) {

      def as[T: NodeSchema]: T =
        implicitly[NodeSchema[T]].decode(
          new Decoder {
            override def apply[X](key: String): X = orientVertex.getProperty[X](key)
          }
        )

      def removeAsync(): Future[Done] =
        blockingOperation(orientVertex.remove()).map(_ => Done)

      def inEdges(edgeClass: Option[String]): Source[Edge, NotUsed] =
        edges(Direction.IN, edgeClass)

      def outEdges(edgeClass: Option[String]): Source[Edge, NotUsed] =
        edges(Direction.OUT, edgeClass)

      def allEdges(edgeClass: Option[String]): Source[Edge, NotUsed] =
        edges(Direction.BOTH, edgeClass)

      private def edges(direction: Direction, edgeClass: Option[String]): Source[Edge, NotUsed] =
        blockingIteratorSource(edgeClass match {
          case Some(value) => () => orientVertex.getEdges(direction, value).iterator().asScala
          case None        => () => orientVertex.getEdges(direction).iterator().asScala
        })
    }

    implicit class EdgeOps(edge: Edge) {

      def removeAsync(): Future[Done] =
        blockingOperation(edge.remove()).map(_ => Done)

      def srcAsync(): Future[OrientVertex] =
        blockingOperation(edge.getVertex(Direction.IN)).mapTo[OrientVertex]

      def destAsync(): Future[OrientVertex] =
        blockingOperation(edge.getVertex(Direction.OUT)).mapTo[OrientVertex]
    }
  }

}

object OrientDBGraph {

  sealed trait Error
  case class ThrowableError(throwable: Throwable) extends Error

  sealed abstract class Location
  case object InMemory extends Location
  case class Local(path: Path) extends Location

  def apply(schema: GraphSchema, location: Location)(implicit system: ActorSystem): OrientDBGraph = {
    val factory = location match {
      case InMemory =>
        new OrientGraphFactory(s"memory:blockchain")

      case Local(path) =>
        new OrientGraphFactory(s"plocal:$path")
    }
    new OrientDBGraph(schema, factory)
  }

  sealed abstract class NodeReference
  case class QueryNodeReference(query: GraphQuery[_]) extends NodeReference
  case class VertexNodeReference(orientVertex: OrientVertex) extends NodeReference
}
