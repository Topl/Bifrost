package co.topl.tools.exporter

import org.mongodb.scala.{Document, MongoClient, MongoDatabase}

class MongoExport(uri: String) extends Exportable {

  override type T = MongoDatabase
  override type C = MongoClient

  def open(uri: String): MongoClient = MongoClient(uri)

  override def writer(db: String): MongoDatabase = open(uri).getDatabase(db)

  override def insert(db: MongoDatabase, ele: String): Unit = db.getCollection("blocks").insertOne(Document(ele))

}
