package co.topl.genusLibrary.orientDb

import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}

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
case class Property(name: String, propertyType: OType)

/**
 * Describe an index on vertices of a class
 *
 * @param name The name of the index
 * @param indexType the type of index @see [[INDEX_TYPE.UNIQUE]], [[INDEX_TYPE.NOTUNIQUE]], ...
 * @param propertyName The names of the properties whose values are used to construct index entries.
 */
case class Index(name: String, indexType: INDEX_TYPE, propertyName: String *)

class DecodeHelper(properties: Map[String, AnyRef]) {
  def apply[T](name: String): T = properties(name).asInstanceOf[T]
}

case class GraphDataEncoder[T] private (
                                         encode:     T => Map[String, AnyRef],
                                         properties: Set[Property],
                                         indices:    Set[Index]
                                       ) {

  def withProperty[V <: AnyRef: OrientDbTyped](
                                                   name:    String,
                                                   extract: T => V,
                                                   indices: Set[(String, OClass.INDEX_TYPE)] = Set.empty
                                                 ): GraphDataEncoder[T] =
    copy(
      t => encode(t).updated(name, extract(t)),
      properties.incl(Property(name, OrientDbTyped[V].oType)),
      this.indices ++ indices.map { case (iName, iType) => Index(iName, iType, name) }
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

    implicit def optionalOrientDbTyped[T: OrientDbTyped]: OrientDbTyped[Option[T]] =
      create(implicitly[OrientDbTyped[T]].oType)
  }
  object Instances extends Instances
}

@simulacrum.typeclass
trait OrientDbIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}
