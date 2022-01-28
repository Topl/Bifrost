package co.topl.genus.algebras

import org.mongodb.scala.bson.BsonTimestamp
import org.mongodb.scala.bson.conversions.Bson

trait MongoOplogAlg[F[_]] {
  def getFirstDocTimestamp(databaseName: String, collectionName: String): F[Option[BsonTimestamp]]

  def getFirstMatchingDocTimestamp(
    databaseName:   String,
    collectionName: String,
    matching:       Bson
  ): F[Option[BsonTimestamp]]
}
