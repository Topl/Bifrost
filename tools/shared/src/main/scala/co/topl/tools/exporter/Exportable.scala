package co.topl.tools.exporter

import co.topl.utils.mongodb.DocumentEncoder

import scala.concurrent.Future

trait Exportable {

  def dataType: DataType

  // Format and write a new row/document/etc
  def insert[T: DocumentEncoder](elements: Seq[T]): Future[_]

  // close connection to writer
  def close(): Unit

}
