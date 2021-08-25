package co.topl.tools.exporter

import scala.concurrent.Future

trait Exportable {

  type T <: Future[_]

  def dataType: DataType

  // Format and write a new row/document/etc
  def insert(element: Seq[String]): T

  // close connection to writer
  def close(): Unit

}
