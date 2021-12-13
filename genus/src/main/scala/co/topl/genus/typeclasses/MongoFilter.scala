package co.topl.genus.typeclasses

import org.mongodb.scala.bson.conversions.Bson
import simulacrum.typeclass

@typeclass trait MongoFilter[T] {
  def toFilter(filter: T): Bson
}
