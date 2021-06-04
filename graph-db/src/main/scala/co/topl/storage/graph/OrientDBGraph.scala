package co.topl.storage.graph

import akka.actor.ActorSystem
import akka.dispatch.Dispatchers
import akka.stream.scaladsl.{Sink, Source}
import akka.stream.{ActorAttributes, Attributes}
import akka.{Done, NotUsed}
import com.orientechnologies.orient.core.sql.OCommandSQL
import com.tinkerpop.blueprints.impls.orient._
import com.tinkerpop.blueprints.{Direction, Edge}

import java.nio.file.Path
import scala.concurrent.Future
import scala.jdk.CollectionConverters._

class OrientDBGraph(schema: GraphSchema, factory: OrientGraphFactory)(implicit system: ActorSystem) {

  import system.dispatcher

  private val blockingDispatcher = system.dispatchers.lookup(Dispatchers.DefaultBlockingDispatcherId)
  var session: OrientGraphNoTx = _

  initialize()

  def initialize(): Unit = {
    session = factory.getNoTx
    initializeSchemas()

    system.registerOnTermination {
      session.shutdown()
      factory.close()
    }
  }

  def blockingIteratorQuery(query: String): Iterator[OrientElement] =
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
        blockingIteratorQuery(query).next().asInstanceOf[OrientVertex]
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
          nodeSchema.srcEdges.foreach(edgeSchema => vertexType.createEdgeProperty(Direction.OUT, edgeSchema.name))
          nodeSchema.destEdges.foreach(edgeSchema => vertexType.createEdgeProperty(Direction.IN, edgeSchema.name))
      }
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

  def resultSetQuery[R <: OrientElement](query: String): Source[R, NotUsed] =
    blockingIteratorSource(() =>
      session
        .command(new OCommandSQL(query))
        .execute[OrientDynaElementIterable]()
        .iterator()
        .asScala
        .collect { case r: R => r }
    )

  def getNode[T: NodeSchema](query: String): Future[Option[T]] = {
    import Ops._
    resultSetQuery[OrientVertex](query)
      .map(_.as[T])
      .runWith(Sink.headOption)
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

  trait Error

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
  case class QueryNodeReference(query: String) extends NodeReference
  case class VertexNodeReference(orientVertex: OrientVertex) extends NodeReference
}
