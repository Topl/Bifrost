package co.topl.genusLibrary.orientDb

import co.topl.models.TypedIdentifier
import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.orientechnologies.orient.core.metadata.schema.{OClass, OPropertyAbstractDelegate, OType}
import scodec.bits.ByteVector

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

  def create[T](name: String, encoder: GraphDataEncoder[T], decode: DecodeHelper => T): VertexSchema[T] = {
    val _name = name
    val _decode = decode
    new VertexSchema[T] {
      def name: String = _name

      def encode(t: T): Map[String, AnyRef] = encoder.encode(t)

      def decode(properties: Map[String, AnyRef]): T = _decode(new DecodeHelper(properties))

      val properties: Set[Property] = encoder.properties

      val indices: Set[Index] = encoder.indices
    }
  }
}

/**
 * Represents an individual piece of data that will be stored in a class of vertices
 * @param name The name of the property
 * @param propertyType The datatype of the property
 */
case class Property(
  name:                    String,
  propertyType:            OType,
  propertyAttributeSetter: OPropertyAbstractDelegate => Unit = f => ()
)

/**
 * Describe an index on vertices of a class
 *
 * @param name The name of the index
 * @param indexType the type of index @see [[INDEX_TYPE.UNIQUE]], [[INDEX_TYPE.NOTUNIQUE]], ...
 * @param propertyNames The names of the properties whose values are used to construct index entries.
 */
case class Index(name: String, indexType: INDEX_TYPE, propertyNames: String*)

class DecodeHelper(properties: Map[String, AnyRef]) {
  def apply[T](name: String): T = properties(name).asInstanceOf[T]
}

/**
 * Describes how instances of a Scala class will be represented as a type of Vertex or edge with properties
 *
 * @param encode A function that extracts data from the object and returns a map of values that can be used as the
 *               vertex's or node's property values.
 * @param properties Describes the properties that will be stores in the vertexes or edges.
 * @param indices Describes the indexes that will be on the type of  vertex or edge.
 * @tparam T the Scala class whose instances are to be represented.
 */
case class GraphDataEncoder[T] private (
  encode:     T => Map[String, AnyRef],
  properties: Set[Property],
  indices:    Set[Index]
) {

  /**
   * Describe a property of the vertex or edge
   *
   * @param name The name of the property
   * @param extract a function to extract the value of the property from an instance of T
   * @param propertyAttributeSetter A function to set the attributes of the property
   * @tparam V The type of value that the property will have
   * @return an updated copy of the GraphDataEncoder
   */
  def withProperty[V <: AnyRef: OrientDbTyped](
    name:                    String,
    extract:                 T => V,
    propertyAttributeSetter: OPropertyAbstractDelegate => Unit = _ => ()
  ): GraphDataEncoder[T] =
    copy(
      t => encode(t).updated(name, extract(t)),
      properties.incl(Property(name, OrientDbTyped[V].oType, propertyAttributeSetter)),
      this.indices
    )

  /**
   * Describe an index on the vertex or edge.
   *
   * @param name The name of the index
   * @param indexType The type of index (INDEX_TYPE.UNIQUE, INDEX_TYPE.NOTUNIQUE, ...)
   * @param propertyNames the names of the properties whose values will be included in the index.
   * @return the updated GraphDataEncoder
   */
  def withIndex(name: String, indexType: INDEX_TYPE, propertyNames: String*): GraphDataEncoder[T] =
    copy(
      encode,
      properties,
      this.indices + Index(name, indexType, propertyNames: _*)
    )
}

object GraphDataEncoder {
  def apply[T]: GraphDataEncoder[T] = GraphDataEncoder(_ => Map.empty, Set.empty, Set.empty)
}

@simulacrum.typeclass
trait OrientDbTyped[T] {
  def oType: OType
}

object OrientDbTyped {

  def create[T](tType: OType): OrientDbTyped[T] =
    new OrientDbTyped[T] {
      def oType: OType = tType
    }

  trait Instances {
    implicit val stringOrientDbTyped: OrientDbTyped[String] = create(OType.STRING)
    implicit val booleanOrientDbTyped: OrientDbTyped[java.lang.Boolean] = create(OType.BOOLEAN)
    implicit val shortOrientDbTyped: OrientDbTyped[java.lang.Short] = create(OType.SHORT)
    implicit val longOrientDbTyped: OrientDbTyped[java.lang.Long] = create(OType.LONG)
    implicit val byteOrientDbTyped: OrientDbTyped[java.lang.Byte] = create(OType.BYTE)
    implicit val byteArrayOrientDbTypes: OrientDbTyped[Array[Byte]] = create(OType.BINARY)

    implicit def optionalOrientDbTyped[T: OrientDbTyped]: OrientDbTyped[Option[T]] =
      create(implicitly[OrientDbTyped[T]].oType)
  }
  object Instances extends Instances
}

@simulacrum.typeclass
trait OrientDbIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}
