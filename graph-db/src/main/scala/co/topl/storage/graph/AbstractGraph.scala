package co.topl.storage.graph

import akka.stream.scaladsl.Source
import akka.{Done, NotUsed}
import cats.data.EitherT

import scala.concurrent.Future

trait ReadableGraph {
  def getNode[T: NodeSchema](graphQuery:  GraphQuery[T]): EitherT[Future, OrientDBGraph.Error, Option[T]]
  def getNodes[T: NodeSchema](graphQuery: GraphQuery[T]): Source[Either[OrientDBGraph.Error, T], NotUsed]
}

trait WritableGraph {
  def insertNode[T: NodeSchema](node: T): EitherT[Future, OrientDBGraph.Error, Done]

  def insertEdge[T, S, D](edge: T, srcRef: OrientDBGraph.NodeReference[S], destRef: OrientDBGraph.NodeReference[D])(
    implicit schema:            EdgeSchema[T, S, D]
  ): EitherT[Future, OrientDBGraph.Error, Done]

  def deleteEdges[T]()(implicit
    schema: EdgeSchema[T, _, _]
  ): EitherT[Future, OrientDBGraph.Error, Done]
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

  def insertEdge[T, S, D](edge: T, srcRef: OrientDBGraph.NodeReference[S], destRef: OrientDBGraph.NodeReference[D])(
    implicit schema:            EdgeSchema[T, S, D]
  ): EitherT[Future, OrientDBGraph.Error, Done] = writableGraph.insertEdge(edge, srcRef, destRef)

  override def deleteEdges[T]()(implicit schema: EdgeSchema[T, _, _]): EitherT[Future, OrientDBGraph.Error, Done] =
    writableGraph.deleteEdges()
}
