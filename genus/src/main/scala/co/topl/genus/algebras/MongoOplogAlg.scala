package co.topl.genus.algebras

import org.mongodb.scala.bson.BsonTimestamp

trait MongoOplogAlg[F[_]] {
  def getFirstDocumentTimestamp(databaseName: String, collectionName: String): F[Option[BsonTimestamp]]
}
