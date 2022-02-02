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

  private[handlers] def checkAddresses(
    keys:  List[Address],
    state: StateReader[_, Address]
  ): Either[RpcError, List[Address]] =
    for {
      _ <- Either.cond(state.hasTBR, {}, ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node"))
      _ <- Either.cond(
        state.nodeKeys.forall(_.intersect(keys.toSet).nonEmpty),
        {},
        ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node")
      )
    } yield keys

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

//  private[handlers] def checkBlocksFoundWithIds(
//    blockOptions: List[Option[Block]],
//  ): Either[RpcError, List[Block]] = blockOptions.sequence.toRight(ToplRpcErrors.NoBlockWithId)
//
//  private[handlers] def checkTxFoundWithIds[T](
//    ids:       List[ModifierId],
//    txOptions:  List[Option[T]],
//    sizeLimit: Int
//  ): Either[RpcError, List[T]] =
//    for {
//      _ <- Either.cond(
//        ids.size <= sizeLimit,
//        {},
//        ToplRpcErrors.unsupportedOperation("Number of ids given exceeded txRetrievalLimit")
//      )
//      txs <- txOptions.sequence.toRight(ToplRpcErrors.NoTransactionWithId)
//    } yield txs

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
