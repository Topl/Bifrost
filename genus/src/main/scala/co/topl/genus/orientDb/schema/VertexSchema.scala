package co.topl.genus.orientDb.schema

import com.tinkerpop.blueprints.Vertex
import com.tinkerpop.blueprints.impls.orient.OrientVertex

/**
 * Describe how data from a scala class will be stored in an OrientDB vertex.
 */
trait VertexSchema[T] {

  /**
   * The name of the vertex class
   */
  def name: String

  /**
   * The properties (fields) that will be stored for the vertex class.
   */
  def properties: Set[Property]

  /**
   * Descriptions of the indexes on the vertex class
   */
  def indices: Set[Index]

  /**
   * Descriptions of the links on the vertex class
   */
  def links: Set[Link]

  /**
   * Encode an object of type T as a Map that OrientDb knows how to store as the properties of a vertex.
   *
   * @param t The object to be encoded
   * @return the map that is the encoding of t
   */
  def encode(t: T): Map[String, AnyRef]

  /**
   * Decode a Map retrieved from the properties an OrientDB vertex to an instance of T
   *
   * @param vertex The underlying graph vertex
   * @return The instance of T constructed from the property values.
   */
  def decode(vertex: Vertex): T
}

object VertexSchema {

  class DecodeHelper(val vertex: Vertex) extends AnyVal {
    def apply[T](name: String): T = vertex.getProperty[T](name)
  }

  def create[T](schemaName: String, encoder: GraphDataEncoder[T], decoder: DecodeHelper => T): VertexSchema[T] =
    new VertexSchema[T] {
      def name: String = schemaName

      def encode(t: T): Map[String, AnyRef] = encoder.encode(t)

      def decode(vertex: Vertex): T = {
        vertex match {
          case o: OrientVertex => scala.util.Try(o.reload())
          case _               =>
        }
        decoder(new DecodeHelper(vertex))
      }

      val properties: Set[Property] = encoder.properties

      val indices: Set[Index] = encoder.indices

      val links: Set[Link] = encoder.links
    }
}
