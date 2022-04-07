package co.topl.genus.typeclasses

import org.mongodb.scala.bson.conversions.Bson

@simulacrum.typeclass
trait MongoSort[T] {
  def toBsonSorting(value: T): Bson
}
