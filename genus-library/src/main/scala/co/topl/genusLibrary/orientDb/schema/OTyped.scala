package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.db.record.OIdentifiable
import com.orientechnologies.orient.core.metadata.schema.OType

@simulacrum.typeclass
trait OTyped[T] {
  def oType: OType
}

object OTyped {

  def create[T](tType: OType): OTyped[T] =
    new OTyped[T] {
      def oType: OType = tType
    }

  trait Instances {
    implicit val stringOrientDbTyped: OTyped[String] = create(OType.STRING)
    implicit val booleanOrientDbTyped: OTyped[java.lang.Boolean] = create(OType.BOOLEAN)
    implicit val shortOrientDbTyped: OTyped[java.lang.Short] = create(OType.SHORT)
    implicit val intOrientDbTyped: OTyped[java.lang.Integer] = create(OType.INTEGER)
    implicit val longOrientDbTyped: OTyped[java.lang.Long] = create(OType.LONG)
    implicit val byteOrientDbTyped: OTyped[java.lang.Byte] = create(OType.BYTE)
    implicit val byteArrayOrientDbTypes: OTyped[Array[Byte]] = create(OType.BINARY)
    implicit val linkOrientDbTypes: OTyped[OIdentifiable] = create(OType.LINK)

    implicit def optionalOrientDbTyped[T: OTyped]: OTyped[Option[T]] =
      create(implicitly[OTyped[T]].oType)
  }
  object Instances extends Instances
}
