package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.db.record.OIdentifiable
import com.orientechnologies.orient.core.metadata.schema.OType

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
    implicit val intOrientDbTyped: OrientDbTyped[java.lang.Integer] = create(OType.INTEGER)
    implicit val longOrientDbTyped: OrientDbTyped[java.lang.Long] = create(OType.LONG)
    implicit val byteOrientDbTyped: OrientDbTyped[java.lang.Byte] = create(OType.BYTE)
    implicit val byteArrayOrientDbTypes: OrientDbTyped[Array[Byte]] = create(OType.BINARY)
    implicit val linkOrientDbTypes: OrientDbTyped[OIdentifiable] = create(OType.LINK)

    implicit def optionalOrientDbTyped[T: OrientDbTyped]: OrientDbTyped[Option[T]] =
      create(implicitly[OrientDbTyped[T]].oType)
  }
  object Instances extends Instances
}
