package co.topl.tools.exporter

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.Filters.{and, gte, lte}
import org.mongodb.scala.model.Projections
import org.mongodb.scala.result.InsertManyResult

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

  def getExistingHeights(start: Long, end: Long): Future[Seq[Long]] = db
    .getCollection(DataType.Block.name)
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
