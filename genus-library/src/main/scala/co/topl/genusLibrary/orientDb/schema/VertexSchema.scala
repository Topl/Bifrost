package co.topl.genusLibrary.orientDb.schema

import com.tinkerpop.blueprints.Vertex
import scala.jdk.CollectionConverters._

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
   * @param properties A map that contains the property values from a vertex.
   * @return The instance of T constructed from the property values.
   */
  def decode(properties: Map[String, AnyRef]): T

  /**
   * Decode a  an OrientDB vertex to an instance of T
   *
   * @param v A vertex.
   * @return The instance of T constructed from the property values.
   */
  def decodeVertex(v: Vertex): T = decode(v.getPropertyKeys.asScala.map(k => (k, v.getProperty(k))).toMap)
}

object VertexSchema {

  class DecodeHelper(properties: Map[String, AnyRef]) {
    def apply[T](name: String): T = properties(name).asInstanceOf[T]
  }

  def create[T](schemaName: String, encoder: GraphDataEncoder[T], decoder: DecodeHelper => T): VertexSchema[T] =
    new VertexSchema[T] {
      def name: String = schemaName

      def encode(t: T): Map[String, AnyRef] = encoder.encode(t)

      def decode(properties: Map[String, AnyRef]): T = decoder(new DecodeHelper(properties))

      val properties: Set[Property] = encoder.properties

      val indices: Set[Index] = encoder.indices

      val links: Set[Link] = encoder.links
    }
}
