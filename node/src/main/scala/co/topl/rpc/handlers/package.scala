package co.topl.rpc

import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.{CustomError, RpcError, ThrowableData}
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.StateReader
import co.topl.nodeView.{NodeViewHolderInterface, ReadableNodeView}
import io.circe.Encoder

import scala.concurrent.{ExecutionContext, Future}
import scala.language.existentials

package object handlers {

  private[handlers] def checkModifierIdType(
    idType: ModifierTypeId,
    ids:    List[ModifierId]
  ): Either[RpcError, List[ModifierId]] = {
    val modifierTypeName = idType match {
      case Transaction.modifierTypeId => "Transaction"
      case Block.modifierTypeId       => "Block"
    }
    Either.cond(
      ids.forall(_.getModType == idType),
      ids,
      ToplRpcErrors.unsupportedOperation(s"The requested id's type is not an id type for $modifierTypeName")
    )
  }

  private[handlers] def checkModifierRetrievalLimit(
    ids:       List[ModifierId],
    sizeLimit: Int
  ): Either[CustomError, List[ModifierId]] =
    Either.cond(
      ids.size <= sizeLimit,
      ids,
      ToplRpcErrors.unsupportedOperation(s"Number of ids given exceeded retrieval limit of $sizeLimit")
    )

  private[handlers] def checkHeightRange(
    bestBlockHeight: Long,
    startHeight:     Long,
    endHeight:       Long,
    sizeLimit:       Int
  ): Either[RpcError, (Long, Long)] =
    for {
      _ <- Either.cond(
        startHeight >= 1 && endHeight >= startHeight && bestBlockHeight >= startHeight,
        {},
        ToplRpcErrors.InvalidHeightRange
      )
      _ <- Either.cond(
        bestBlockHeight >= endHeight,
        {},
        ToplRpcErrors.unsupportedOperation("End height exceeded head height")
      )
      _ <- Either.cond(
        (endHeight - startHeight + 1) <= sizeLimit,
        {},
        ToplRpcErrors.unsupportedOperation("Height range exceeded block/block id retrieval limit")
      )
    } yield (startHeight, endHeight)

  private[handlers] def readFromNodeViewHolder[T](
    nodeViewHolderInterface: NodeViewHolderInterface
  )(f:                       ReadableNodeView => T)(implicit
    throwableEncoder:        Encoder[ThrowableData],
    executionContext:        ExecutionContext
  ): EitherT[Future, RpcError, T] =
    nodeViewHolderInterface
      .withNodeView(f)
      .leftMap { case NodeViewHolderInterface.ReadFailure(throwable) =>
        CustomError.fromThrowable(throwable): RpcError
      }
}
