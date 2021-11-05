package co.topl.tools.exporter

import co.topl.utils.mongodb
import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.Filters.{and, gte, in, lte}
import org.mongodb.scala.model.Projections
import org.mongodb.scala.result.{DeleteResult, InsertManyResult}

import scala.concurrent.Future

class MongoChainRepExport(uri: String, database: String) {

  private val client = open(uri)
  private val db = client.getDatabase(database)

  private def open(uri: String): MongoClient = MongoClient(uri)

  def checkValidConnection(): Future[Seq[String]] = db.listCollectionNames().toFuture()

  def insert(docSeq: Seq[Document], collectionName: String): Future[InsertManyResult] = db
    .getCollection(collectionName)
    .insertMany(docSeq)
    .toFuture()

  def remove(field: String, value: Seq[String], collectionName: String): Future[DeleteResult] = db
    .getCollection(collectionName)
    .deleteMany(in(field, value))
    .toFuture()

  def getUnconfirmedTx(collectionName: String): Future[Seq[String]] = db
    .getCollection(collectionName)
    .find()
    .projection(Projections.fields(Projections.include("txId"), Projections.excludeId()))
    .map(_.head._2.toString)
    .toFuture()

  def getMissingBlockIds(idsToCheck: String, collectionName: String): Future[Seq[String]] = db
    .getCollection(collectionName)
    .find(in("id", idsToCheck))
    .projection(Projections.fields(Projections.include("id"), Projections.excludeId()))
    .map(_.head._2.toString)
    .toFuture()

  def getExistingHeights(start: Long, end: Long, collectionName: String): Future[Seq[Long]] = db
    .getCollection(collectionName)
    .find(and(gte("height", start), lte("height", end)))
    .projection(Projections.fields(Projections.include("height"), Projections.excludeId()))
    .map(_.head._2.asNumber().longValue())
    .toFuture()

  def close(): Unit = client.close()

}

object MongoChainRepExport {

  def apply(uri: String, db: String): MongoChainRepExport =
    new MongoChainRepExport(uri, db)
}
