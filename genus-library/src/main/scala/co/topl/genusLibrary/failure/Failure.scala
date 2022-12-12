package co.topl.genusLibrary.failure

import co.topl.models.TypedIdentifier
import scodec.bits.ByteVector

import scala.collection.immutable.ListSet

sealed abstract class Failure(message: String, exception: Option[Exception] = None)

object Failures {

  case class NoPreviousHeaderVertexFailure(blockId: (Byte, ByteVector), parentHeaderId: TypedIdentifier)
      extends Failure(
        s"Block doesn't have a previous header vertex. blockId=[$blockId] parentHeaderId=[$parentHeaderId]"
      )

  case class MissingInputVerticesFailure(inputs: Set[Set[(String, AnyRef)]]) extends Failure(
    s"One or more input vertices are missing. QueryParameters=$inputs"
  )

  case class NoBlockHeaderFoundOnNodeFailure(blockId: TypedIdentifier)
      extends Failure(s"Block header wasn't found. BlockId=[$blockId]")

  case class NoBlockBodyFoundOnNodeFailure(blockId: TypedIdentifier)
      extends Failure(s"Block body wasn't found. BlockId=[$blockId]")

  case class NonExistentTransactionsFailure(transactions: ListSet[TypedIdentifier])
      extends Failure(s"Transactions weren't found. Transactions=[$transactions]")
}
