package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OClass.INDEX_TYPE
import com.orientechnologies.orient.core.metadata.schema.OPropertyAbstractDelegate

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
    propertyAttributeSetter: OPropertyAbstractDelegate => Unit
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
