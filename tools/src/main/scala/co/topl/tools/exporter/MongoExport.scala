package co.topl.tools.exporter

import org.mongodb.scala.model.Filters.{and, gte, lte}
import org.mongodb.scala.model.Projections
import org.mongodb.scala.result.InsertManyResult
import org.mongodb.scala.{Document, MongoClient, MongoDatabase}

import scala.concurrent.Future

class MongoExport(uri: String, database: String, collection: String, dt: DataType) extends Exportable {

  private val client = open(uri)
  private val db = createDatabase(database)
  override val dataType: DataType = dt

  private def open(uri: String): MongoClient = MongoClient(uri)

  private def createDatabase(db: String): MongoDatabase = client.getDatabase(db)

  def checkValidConnection(): Future[Seq[String]] = db.listCollectionNames().toFuture()

  override def insert(ele: Seq[String]): Future[InsertManyResult] = db
    .getCollection(collection)
    .insertMany(ele.map(Document(_)))
    .toFuture()

  def getExistingHeights(start: Long, end: Long): Future[Seq[Long]] = db
    .getCollection(collection)
    .find(and(gte("height", start), lte("height", end)))
    .projection(Projections.fields(Projections.include("height"), Projections.excludeId()))
    .map(_.head._2.asInt64().getValue)
    .toFuture()

  override def close(): Unit = client.close()

}

object MongoExport {

  def apply(uri: String, db: String, coll: String, dt: DataType): MongoExport = new MongoExport(uri, db, coll, dt)
}
