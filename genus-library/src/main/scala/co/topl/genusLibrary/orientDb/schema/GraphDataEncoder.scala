package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OType

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
  indices:    Set[Index],
  links:      Set[Link]
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
    name:      String,
    extract:   T => V,
    mandatory: Boolean,
    readOnly:  Boolean,
    notNull:   Boolean
  ): GraphDataEncoder[T] =
    copy(
      encode = t => encode(t).updated(name, extract(t)),
      properties = properties.incl(Property(name, OrientDbTyped[V].oType, mandatory, readOnly, notNull))
    )

  /**
   * Describe an index on the vertex or edge.
   *
   * @param name The name of the index
   * @param propertyNames the names of the properties whose values will be included in the index.
   * @return the updated GraphDataEncoder
   */

  def withIndex[V <: AnyRef: OrientDbIndexable](name: String, propertyNames: String*): GraphDataEncoder[T] =
    copy(indices = indices + Index(name, OrientDbIndexable[V].indexType, propertyNames: _*))

  /**
   * Describe an Link on the vertex
   *
   * @param propertyName Defines the property to link from.
   * @param linkType Defines the type for the link. In the event of an inverse relationship, (the most common), you can specify LINKSET or LINKLIST for 1-n relationships.
   * @param linkedClass Defines the class to link to.
   * @return the updated GraphDataEncoder
   */
  def withLink(propertyName: String, linkType: OType, destClassName: String): GraphDataEncoder[T] =
    copy(
      links = links + Link(propertyName, linkType, destClassName)
    )
}

object GraphDataEncoder {
  def apply[T]: GraphDataEncoder[T] = GraphDataEncoder(_ => Map.empty, Set.empty, Set.empty, Set.empty)
}
