package co.topl.tools.exporter

import org.mongodb.scala.MongoDatabase

import java.io.Closeable

trait Exportable {

  type T
  type C <: Closeable

  // A path to some connection or file that the exporter will write to
  def open(str: String): C

  // The object that is going to write to the file or connection
  def writer(str: String): T

  // Format and write a new row/document/etc
  def insert(connection: T, element: String): Unit

}
