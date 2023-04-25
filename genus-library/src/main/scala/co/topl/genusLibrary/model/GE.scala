package co.topl.genusLibrary.model

import cats.implicits._
import co.topl.typeclasses.implicits._
import co.topl.brambl.models.TransactionId
import co.topl.consensus.models.BlockId
import scala.collection.immutable.ListSet

/**
 * Genus Exception
 * Base exception class for the Genus library.
 * @param message the detail message (which is saved for later retrieval by the getMessage() method).
 */
abstract class GE(message: String) extends Exception(message)

object GEs {

  case class HeaderNotFound(blockId: BlockId) extends GE(s"Block header wasn't found. BlockId=[${blockId.show}]")

  case class BodyNotFound(blockId: BlockId) extends GE(s"Block body wasn't found. BlockId=[${blockId.show}]")

  case class TransactionsNotFound(ioTx32s: ListSet[TransactionId])
      extends GE(s"Transactions weren't found. Transactions=[${ioTx32s.map(_.show)}]")

  case class NotFound(msg: String) extends GE(msg)

  case object UnImplemented extends GE("An implementation is missing")

  case class Internal(cause: Throwable) extends GE(s"${cause.getMessage}") {
    this.initCause(cause)
  }

  case class InternalMessage(msg: String) extends GE(msg)

  case class InternalMessageCause(msg: String, cause: Throwable) extends GE(msg) {
    this.initCause(cause)
  }
}
