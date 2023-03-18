package co.topl.genusLibrary.orientDb.schema

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
}

object VertexSchema {

  class DecodeHelper(properties: Map[String, AnyRef]) {
    def apply[T](name: String): T = properties(name).asInstanceOf[T]
  }

  def create[T](schemaName: String, encoder: GraphDataEncoder[T], decoder: DecodeHelper => T): VertexSchema[T] = {
    new VertexSchema[T] {
      def name: String = schemaName

      def encode(t: T): Map[String, AnyRef] = encoder.encode(t)

      def decode(properties: Map[String, AnyRef]): T = decoder(new DecodeHelper(properties))

      val properties: Set[Property] = encoder.properties

      val indices: Set[Index] = encoder.indices
    }
  }
}
