package co.topl.tools.exporter

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.{BsonString, Document}
import org.mongodb.scala.model.Filters.{and, gte, in, lte}
import org.mongodb.scala.model.Projections
import org.mongodb.scala.result.{DeleteResult, InsertManyResult}

import scala.concurrent.Future

class MongoChainRepExport(uri: String, database: String) extends DatabaseOperations {

  private val client = open(uri)
  private val db = client.getDatabase(database)

  private def open(uri: String): MongoClient = MongoClient(uri)

  def checkValidConnection(): Future[Seq[String]] = db.listCollectionNames().toFuture()

  def insert(docSeq: Seq[Document], collectionName: String): Future[InsertManyResult] = db
    .getCollection(collectionName)
    .insertMany(docSeq)
    .toFuture()

  def remove(field: String, values: Seq[String], collectionName: String): Future[DeleteResult] = {
    val listIds = values.map(BsonString(_))
    db
      .getCollection(collectionName)
      .deleteMany(in(field, listIds: _*))
      .toFuture()
  }

  def getUnconfirmedTxs(collectionName: String): Future[Seq[String]] = db
    .getCollection(collectionName)
    .find()
    .projection(Projections.fields(Projections.include("txId"), Projections.excludeId()))
    .map(_.head._2.asString().getValue)
    .toFuture()

  def getExistingIds(idsToCheck: Seq[String], collectionName: String): Future[Seq[String]] = {
    val listIds = idsToCheck.map(BsonString(_))
    db
      .getCollection(collectionName)
      .find(in("id", listIds: _*))
      .projection(Projections.fields(Projections.include("id"), Projections.excludeId()))
      .map(_.head._2.asString().getValue)
      .toFuture()
  }

  def close(): Unit = client.close()

}

object MongoChainRepExport {

  def apply(uri: String, db: String): MongoChainRepExport =
    new MongoChainRepExport(uri, db)
}
