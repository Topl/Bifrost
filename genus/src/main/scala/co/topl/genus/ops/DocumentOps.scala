package co.topl.genus.ops

import co.topl.genus.types.BlockHeight
import org.mongodb.scala.Document

import scala.language.implicitConversions

import scala.util.Try

final class DocumentOps(private val value: Document) extends AnyVal {

  /**
   * Attempts to get the block height if the underlying document is a representation of a Transaction.
   * @return a [[BlockHeight]] if one is available
   */
  def getTransactionBlockHeight: Option[BlockHeight] =
    value
      .get("block")
      .flatMap(bsonValue => Try(bsonValue.asDocument()).toOption)
      .flatMap(document => Document(document).get("height"))
      .flatMap(bsonValue => Try(bsonValue.asNumber()).toOption)
      .map(number => BlockHeight(number.longValue()))

  /**
   * Attempts to get the block height if the underlying document is a representation of a Block.
   * @return a [[BlockHeight]] if one is available.
   */
  def getBlockHeight: Option[BlockHeight] =
    value
      .get("block")
      .flatMap(bsonValue => Try(bsonValue.asNumber()).toOption)
      .map(number => BlockHeight(number.longValue()))
}

object DocumentOps {

  trait ToOps {
    implicit def documentOpsFromValue(value: Document): DocumentOps = new DocumentOps(value)
  }

  object ops extends ToOps
}
