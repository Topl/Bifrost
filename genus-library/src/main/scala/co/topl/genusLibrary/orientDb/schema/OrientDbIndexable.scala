package co.topl.genusLibrary.orientDb.schema

import com.orientechnologies.orient.core.metadata.schema.OClass

@simulacrum.typeclass
trait OrientDbIndexable[T] {
  def indexType: OClass.INDEX_TYPE
}
