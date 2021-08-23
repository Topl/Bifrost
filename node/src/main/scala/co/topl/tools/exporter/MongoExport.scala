package co.topl.tools.exporter

import org.mongodb.scala.{result, Document, MongoClient, MongoDatabase}

import scala.concurrent.Future

class MongoExport(uri: String, dt: DataType) extends Exportable {

  override type T = Future[_ <: result.InsertManyResult]

  private val client = open(uri)
  private val db = createDatabase("bifrost")
  override val dataType: DataType = dt

  private def open(uri: String): MongoClient = MongoClient(uri)

  private def createDatabase(db: String): MongoDatabase = client.getDatabase(db)

  override def insert(ele: Seq[String]): T = db
    .getCollection(dataType.name)
    .insertMany(ele.map(Document(_)))
    .toFuture()

  override def close(): Unit = client.close()

}

object MongoExport {

  def apply(uri: String, dt: DataType): MongoExport = new MongoExport(uri, dt)
}
