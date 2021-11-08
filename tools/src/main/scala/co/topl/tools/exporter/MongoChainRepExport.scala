package co.topl.tools.exporter

import org.mongodb.scala.MongoClient
import org.mongodb.scala.bson.Document
import org.mongodb.scala.model.Filters.{and, gte, in, lte}
import org.mongodb.scala.model.{Filters, Projections}
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

  def remove(field: String, values: Seq[String], collectionName: String): Future[DeleteResult] = {
    val res = db
      .getCollection(collectionName)
      .deleteMany(in(field, values))
      .toFuture()

    //TODO: Jing - cleanup
    val newres = db
      .getCollection(collectionName)
      .deleteMany(Filters.equal("txId", List("tL9YbQqT7WiGNoNH7at8GdbnYDUNRuctEM4JyLTQbwFu")))
      .toFuture()
    Thread.sleep(1000)
    println(s"${Console.RED_B}$field $values --$res${Console.RESET}")
    println(s"${Console.RED_B}$field $values --$newres${Console.RESET}")
    res
  }

  def getUnconfirmedTx(collectionName: String): Future[Seq[String]] = db
    .getCollection(collectionName)
    .find()
    .projection(Projections.fields(Projections.include("txId"), Projections.excludeId()))
    .map(_.head._2.asString().getValue)
    .toFuture()

  def getMissingBlockIds(idsToCheck: Seq[String], collectionName: String): Future[Seq[String]] = db
    .getCollection(collectionName)
    .find(in("id", idsToCheck))
    .projection(Projections.fields(Projections.include("id"), Projections.excludeId()))
    .map(_.head._2.asString().getValue)
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
