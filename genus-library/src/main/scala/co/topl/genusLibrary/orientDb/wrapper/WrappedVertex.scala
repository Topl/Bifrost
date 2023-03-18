package co.topl.genusLibrary.orientDb.wrapper

import com.tinkerpop.blueprints.Vertex

/**
 * Wrapped Tinkerpop's Orient Vertex Java API. Makes testing easier and more reliable.
 * Method signatures and implementation from the the API and the wrapper are equivalent.
 * @see https://dev.to/satansdeer/dont-mock-what-you-dont-own-cd6
 * @param orientVertex Tinkerpop's Transactional Orient Vertex Java API
 */
class WrappedVertex(val orientVertex: Vertex) {
  // TODO remove this class and use com.tinkerpop.blueprints.util.wrappers.wrapped.WrappedVertex in case we need a wrapper

  private[wrapper] def get: Vertex = orientVertex

  def setProperty(key: String, value: AnyRef): Unit = orientVertex.setProperty(key, value)

}
