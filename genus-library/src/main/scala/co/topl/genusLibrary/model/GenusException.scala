package co.topl.genusLibrary.model

import co.topl.brambl.models.Identifier
import co.topl.consensus.models.BlockId
import scala.collection.immutable.ListSet
import scodec.bits.ByteVector

/**
 * Base exception class for the Genus library.
 * @param message the detail message (which is saved for later retrieval by the getMessage() method).
 * @param cause the cause (which is saved for later retrieval by the getCause() method). (A null value is permitted, and indicates that the cause is nonexistent or unknown.)
 */
abstract class GenusException(message: String, cause: Throwable) extends RuntimeException(message, cause)

object GenusExceptions {

  case class NoCurrentHeaderVertex(blockId: ByteVector)
      extends GenusException(
        s"Block doesn't have a header vertex. blockId=[$blockId]",
        new IllegalStateException("Undefined cause")
      )

  case class NoPreviousHeaderVertex(blockId: ByteVector)
      extends GenusException(
        s"Block doesn't have a previous header vertex. Previous blockId=[$blockId]",
        new IllegalStateException("Undefined cause")
      )

  case class NoCurrentBodyVertex(blockId: ByteVector)
      extends GenusException(
        s"Block doesn't have a body vertex. blockId=[$blockId]",
        new IllegalStateException("Undefined cause")
      )

  case class NoBlockHeaderFoundOnNode(blockId: BlockId)
      extends GenusException(
        s"Block header wasn't found. BlockId=[$blockId]",
        new IllegalStateException("Undefined cause")
      )

  case class NoBlockBodyFoundOnNode(blockId: BlockId)
      extends GenusException(
        s"Block body wasn't found. BlockId=[$blockId]",
        new IllegalStateException("Undefined cause")
      )

  case class NonExistentTransactions(transactions: ListSet[Identifier.IoTransaction32])
      extends GenusException(
        s"Transactions weren't found. Transactions=[$transactions]",
        new IllegalStateException("Undefined cause")
      )

  case class OrientCommitException(ex: RuntimeException)
      extends GenusException("There was an error while committing Orient Transaction", ex)

  case class Message(msg: String) extends GenusException(msg, new IllegalStateException("Undefined cause"))
  case class MessageWithCause(msg: String, cause: Throwable) extends GenusException(msg, cause)

  case class NotFound(msg: String) extends GenusException(msg, new IllegalStateException("Not found"))
  case object UnImplemented extends GenusException("UnImplemented", new NotImplementedError("UnImplemented"))
  case class Internal(e: Throwable) extends GenusException("Internal Error", e)
}
