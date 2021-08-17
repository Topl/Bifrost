package co.topl.tools.exporter

import scala.concurrent.Future

trait Exportable {

  type T

  // Format and write a new row/document/etc
  def insert(element: String): T

}
