package co.topl.genusLibrary.model

import co.topl.brambl.models.Identifier
import co.topl.consensus.models.BlockId
import scala.collection.immutable.ListSet

/**
 * Genus Runtime Exception
 * Base exception class for the Genus library.
 * @param message the detail message (which is saved for later retrieval by the getMessage() method).
 * @param cause the cause (which is saved for later retrieval by the getCause() method). (A null value is permitted, and indicates that the cause is nonexistent or unknown.)
 */
abstract class GRE(message: String, cause: Throwable) extends RuntimeException(message, cause)

object GREs {

  case class HeaderNotFound(blockId: BlockId)
      extends GRE(s"Block header wasn't found. BlockId=[$blockId]", new IllegalStateException("Undefined cause"))

  case class BodyNotFound(blockId: BlockId)
      extends GRE(s"Block body wasn't found. BlockId=[$blockId]", new IllegalStateException("Undefined cause"))

  case class NonExistentTransactions(transactions: ListSet[Identifier.IoTransaction32])
      extends GRE(
        s"Transactions weren't found. Transactions=[$transactions]",
        new IllegalStateException("Undefined cause")
      )

  case class Message(msg: String) extends GRE(msg, new IllegalStateException("Undefined cause"))
  case class MessageCause(msg: String, cause: Throwable) extends GRE(msg, cause)

  case class NotFound(msg: String) extends GRE(msg, new IllegalStateException("Not found"))
  case object UnImplemented extends GRE("UnImplemented", new NotImplementedError("UnImplemented"))
  case class Internal(e: Throwable) extends GRE("Internal Error", e)
}
