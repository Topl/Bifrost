package co.topl.genus.interpreters

import co.topl.genus.types.{Block, Transaction}
import co.topl.utils.mongodb.DocumentDecoder
import co.topl.utils.mongodb.models.{BlockDataModel, ConfirmedTransactionDataModel}
import org.mongodb.scala.Document
import co.topl.utils.mongodb.codecs._
import co.topl.genus.typeclasses.implicits._

package object services {

  /**
   * Attempts to convert the given Mongo [[Document]] to a [[Transaction]] value.
   * @param document the document to convert
   * @return if successful, a [[Transaction]] value, otherwise a [[String]] error message
   */
  def documentToTransaction(document: Document): Either[String, Transaction] =
    DocumentDecoder[ConfirmedTransactionDataModel].fromDocument(document).map(_.transformTo[Transaction])

  /**
   * Attempts to convert the given Mongo [[Document]] to a [[Block]] value.
   * @param document the document to convert
   * @return if successful, a [[Transaction]] value, otherwise a [[String]] error message
   */
  def documentToBlock(document: Document): Either[String, Block] =
    DocumentDecoder[BlockDataModel].fromDocument(document).map(_.transformTo[Block])
}
