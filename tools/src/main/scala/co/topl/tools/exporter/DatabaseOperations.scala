package co.topl.tools.exporter

import com.mongodb.client.result.{DeleteResult, InsertManyResult}
import org.mongodb.scala.bson.Document

import scala.concurrent.Future

trait DatabaseOperations {
  def checkValidConnection(): Future[Seq[String]]
  def insert(docSeq: Seq[Document], collectionName: String): Future[InsertManyResult]
  def remove(field:  String, values:                Seq[String], collectionName: String): Future[DeleteResult]
  def getUnconfirmedTxs(collectionName: String): Future[Seq[String]]
  def getExistingIds(idsToCheck:        Seq[String], collectionName: String): Future[Seq[String]]
}
