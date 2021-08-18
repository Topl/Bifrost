package co.topl.tools.exporter

import org.mongodb.scala.{result, Document, MongoClient, MongoDatabase}

import scala.concurrent.Future

class MongoExport(uri: String) extends Exportable {

  override type T = Future[result.InsertOneResult]

  private val client = open(uri)
  private val db = createDatabase("bifrost")

  private def open(uri: String): MongoClient = MongoClient(uri)

  private def createDatabase(db: String): MongoDatabase = client.getDatabase(db)

  override def insert(ele: String): T = db
    .getCollection("blocks")
    .insertOne(Document(ele))
    .toFuture()

  override def close(): Unit = client.close()

}

object MongoExport {

  def apply(uri: String): MongoExport = new MongoExport(uri)
}
