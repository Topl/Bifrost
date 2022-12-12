package co.topl.genusLibrary.orientDb.wrapper

import co.topl.genusLibrary.orientDb.VertexSchema
import com.tinkerpop.blueprints.{Edge, Vertex}

sealed trait GenusBatchGraph

class OpenForCommitsBatchGraph(batchGraph: OrientBatchGraph) extends GenusBatchGraph {

  def createVertex[T](
    elem: T
  )(implicit
    schema: VertexSchema[T]
  ): (T, Vertex) =
    updateVertex(elem, batchGraph.addVertex(s"class:${schema.name}"))

  def updateVertex[T](
    elem:   T,
    vertex: Vertex
  )(implicit
    schema: VertexSchema[T]
  ): (T, Vertex) = {
    schema
      .encode(elem)
      .foreach { case (key, value) =>
        vertex.setProperty(key, value)
      }

    (elem, vertex)
  }

  def addEdge(
    outVertex: Vertex,
    inVertex:  Vertex,
    label:     Option[String] = None
  ): Edge =
    batchGraph.addEdge(null, outVertex, inVertex, label.orNull)

  def commit(): TerminatedBatchGraph.type = {
    batchGraph.commit()
    TerminatedBatchGraph
  }
}

object TerminatedBatchGraph extends GenusBatchGraph
