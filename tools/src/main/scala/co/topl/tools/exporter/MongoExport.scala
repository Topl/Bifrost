package co.topl.tools.exporter

import co.topl.utils.mongodb.DocumentEncoder
import org.mongodb.scala.result.InsertManyResult
import org.mongodb.scala.{Document, MongoClient, MongoDatabase}
import co.topl.utils.mongodb.implicits._

import scala.concurrent.Future

class MongoExport(uri: String, database: String, collection: String, dt: DataType) extends Exportable {

  private val client = open(uri)
  private val db = createDatabase(database)
  override val dataType: DataType = dt

  private def open(uri: String): MongoClient = MongoClient(uri)

  private def createDatabase(db: String): MongoDatabase = client.getDatabase(db)

  override def insert[T: DocumentEncoder](ele: Seq[T]): Future[InsertManyResult] = db
    .getCollection(collection)
    .insertMany(ele.map(_.asDocument))
    .toFuture()

  override def close(): Unit = client.close()

}

object MongoExport {

  def apply(uri: String, db: String, coll: String, dt: DataType): MongoExport = new MongoExport(uri, db, coll, dt)
}
