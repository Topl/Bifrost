package co.topl.genus.interpreters.orientdb

import cats.data.Chain
import com.orientechnologies.orient.core.metadata.schema.{OClass, OType}
import com.tinkerpop.blueprints.impls.orient.OrientVertex

@simulacrum.typeclass
trait NodeSchema[T] {
  def name: String
  def properties: Set[Property]
  def indices: Set[Index]
  def encode(t:          T): Map[String, AnyRef]
  def decode(properties: Map[String, AnyRef]): T
}

object NodeSchema {

  def create[T](name: String, encoder: GraphDataEncoder[T], decode: DecodeHelper => T): NodeSchema[T] = {
    val _name = name
    val _decode = decode
    new NodeSchema[T] {
      def name: String = _name

      def encode(t: T): Map[String, AnyRef] = encoder.encode(t)

      def decode(properties: Map[String, AnyRef]): T = _decode(new DecodeHelper(properties))

      val properties: Set[Property] = encoder.properties

      val indices: Set[Index] = encoder.indices
    }
  }
}

/**
 * Represents an individual piece of data
 * @param name The name of the property
 * @param propertyType The datatype of the property
 */
case class Property(name: String, propertyType: OType)
case class Index(name: String, indexType: OClass.INDEX_TYPE, propertyName: String)

class DecodeHelper(properties: Map[String, AnyRef]) {
  def apply[T](name: String): T = properties(name).asInstanceOf[T]
}

case class GraphDataEncoder[T] private (
  encode:     T => Map[String, AnyRef],
  properties: Set[Property],
  indices:    Set[Index]
) {

  def withProperty[V <: AnyRef: OrientDbTypeable](
    name:    String,
    extract: T => V,
    indices: Set[(String, OClass.INDEX_TYPE)] = Set.empty
  ): GraphDataEncoder[T] =
    copy(
      t => encode(t).updated(name, extract(t)),
      properties.incl(Property(name, OrientDbTypeable[V].oType)),
      this.indices ++ indices.map { case (iName, iType) => Index(iName, iType, name) }
    )

}

object GraphDataEncoder {
  def apply[T]: GraphDataEncoder[T] = GraphDataEncoder(_ => Map.empty, Set.empty, Set.empty)
}

@simulacrum.typeclass
trait OrientDbTypeable[T] {
  def oType: OType
}

object OrientDbTypeable {

  def create[T](tType: OType): OrientDbTypeable[T] =
    new OrientDbTypeable[T] {
      def oType: OType = tType
    }

  trait Instances {
    implicit val stringOrientDbTypeable: OrientDbTypeable[String] = create(OType.STRING)
    implicit val booleanOrientDbTypeable: OrientDbTypeable[java.lang.Boolean] = create(OType.BOOLEAN)
    implicit val shortOrientDbTypeable: OrientDbTypeable[java.lang.Short] = create(OType.SHORT)
    implicit val longOrientDbTypeable: OrientDbTypeable[java.lang.Long] = create(OType.LONG)

    implicit def optionalOrientDbTypeable[T: OrientDbTypeable]: OrientDbTypeable[Option[T]] =
      create(implicitly[OrientDbTypeable[T]].oType)
  }
  object instances extends Instances
}

@simulacrum.typeclass
trait OrientDbIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}
