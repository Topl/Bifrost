package co.topl.genusLibrary.failure

import cats.implicits._
import co.topl.models.TypedIdentifier
import scodec.bits.ByteVector

import scala.collection.immutable.ListSet

abstract class Failure(message: String, exception: Option[RuntimeException] = None)

object Failures {

  case class NoCurrentHeaderVertexFailure(blockId: ByteVector)
      extends Failure(
        s"Block doesn't have a header vertex. blockId=[$blockId]"
      )

  case class NoPreviousHeaderVertexFailure(blockId: ByteVector)
      extends Failure(
        s"Block doesn't have a previous header vertex. Previous blockId=[$blockId]"
      )

  case class NoCurrentBodyVertexFailure(transactions: ByteVector)
      extends Failure(
        s"Block doesn't have a transactions vertex. transactions=[$transactions]"
      )

  case class NoBlockHeaderFoundOnNodeFailure(blockId: TypedIdentifier)
      extends Failure(s"Block header wasn't found. BlockId=[$blockId]")

  case class NoBlockBodyFoundOnNodeFailure(blockId: TypedIdentifier)
      extends Failure(s"Block body wasn't found. BlockId=[$blockId]")

  case class NonExistentTransactionsFailure(transactions: ListSet[TypedIdentifier])
      extends Failure(s"Transactions weren't found. Transactions=[$transactions]")

  case class OrientCommitException(ex: RuntimeException)
      extends Failure("There was an error while committing Orient Transaction", ex.some)

}
