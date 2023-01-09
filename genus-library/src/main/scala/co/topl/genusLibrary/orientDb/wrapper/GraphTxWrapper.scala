package co.topl.genusLibrary.orientDb.wrapper

import co.topl.genusLibrary.orientDb.VertexSchema
import com.tinkerpop.blueprints.{Edge, Vertex}
import com.tinkerpop.blueprints.impls.orient.OrientGraph

/**
 * Even though Tinkerpop's API is very high level and great in design for a Java application,
 * there are some use cases that feel a bit low level on our end, may induce duplicate code and/or bad practices.
 * @param graph Tinkerpop's implementation of an TX Orient Graph.
 */
class GraphTxWrapper(graph: OrientGraph) {

  /**
   * Vertex creator under the given graph on instantiation
   *
   * @param elem element to insert
   * @param schema schema for given element to be inserted
   * @tparam T abstract type of the given element and type parameter of given schema
   * @return element and created vertex
   */
  def createVertex[T](
    elem: T
  )(implicit
    schema: VertexSchema[T]
  ): (T, Vertex) =
    updateVertex(elem, graph.addVertex(s"class:${schema.name}"))

  /**
   * Vertex updater from an element
   *
   * @param elem element to update
   * @param vertex vertex to update
   * @param schema schema for given element to be updated
   * @tparam T abstract type of the given element and type parameter of given schema
   * @return element and updated vertex
   */
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

  /**
   * Edge creator. Creates edge from outVertex to inVertex.
   * outVertex -[label]-> inVertex
   *
   * @param outVertex vertex from which the edge starts
   * @param inVertex vertex from which the edge ends
   * @param label description of the edge
   * @return created edge
   */
  def addEdge(
    outVertex: Vertex,
    inVertex:  Vertex,
    label:     Option[String] = None
  ): Edge =
    graph.addEdge(null, outVertex, inVertex, label.orNull)

  /**
   * Commits graph
   */
  def commit(): Unit = graph.commit()

}
