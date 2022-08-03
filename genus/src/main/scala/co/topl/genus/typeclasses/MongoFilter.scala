package co.topl.genus.typeclasses

import org.mongodb.scala.bson.conversions.Bson
import simulacrum.typeclass

@typeclass trait MongoFilter[T] {

  /**
   * Converts the value into a Bson filter for applying to Mongo queries.
   * @param filter the value to convert into a filter
   * @return the Bson value representing the Mongo filter
   */
  def toBsonFilter(filter: T): Bson
}
