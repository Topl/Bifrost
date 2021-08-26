package co.topl.tools.exporter

import scala.concurrent.Future

trait Exportable[+T] {

  def dataType: DataType

  // Format and write a new row/document/etc
  def insert(element: Seq[String]): Future[T]

  // close connection to writer
  def close(): Unit

}
