package co.topl.genusLibrary.orientDb.wrapper

import com.tinkerpop.blueprints.impls.orient.OrientGraph

/**
 * Wrapped Tinkerpop's Transactional Graph Java API. Makes testing easier and more reliable.
 * Method signatures and implementation from the the API and the wrapper are equivalent.
 * @see https://dev.to/satansdeer/dont-mock-what-you-dont-own-cd6
 * @param graph Tinkerpop's Transactional Graph Java API
 */
class GraphTxWrapper(graph: OrientGraph) {

  def addVertex(id: AnyRef): WrappedVertex = new WrappedVertex(graph.addVertex(id))

  def addEdge(id: AnyRef, outVertex: WrappedVertex, inVertex: WrappedVertex, label: String): WrappedEdge =
    new WrappedEdge(graph.addEdge(id, outVertex.get, inVertex.get, label))

  def commit(): Unit = graph.commit()

  def rollback(): Unit = graph.rollback()

}
