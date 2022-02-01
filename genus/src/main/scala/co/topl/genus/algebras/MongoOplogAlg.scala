package co.topl.genus.algebras

import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.bson.conversions.Bson

/**
 * A set of utilities for operating on the Mongo Oplog collection.
 * @tparam F the effect-ful type of the final value
 */
trait MongoOplogAlg[F[_]] {

  /**
   * Gets the timestamp of the first document entered into the collection.
   * @param databaseName the name of the database containing the collection
   * @param collectionName the name of the collection that the document was added to
   * @return the timestamp of the first document if at least one document exists
   */
  def getFirstDocTimestamp(databaseName: String, collectionName: String): F[Option[BsonTimestamp]]

  /**
   * Gets the timestamp of the first document matching the given filter.
   * @param databaseName the name of the database containing the collection
   * @param collectionName the name of the collection that the document was added to
   * @param matching the filter to apply on the oplog documents
   * @return the timestamp of the first matching document if at least one exists
   */
  def getFirstMatchingDocTimestamp(
    databaseName:   String,
    collectionName: String,
    matching:       Bson
  ): F[Option[BsonTimestamp]]
}
