package co.topl.rpc

import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.NodeViewModifier.ModifierTypeId
import co.topl.modifier.block.Block
import co.topl.modifier.transaction.Transaction
import co.topl.nodeView.state.StateReader

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
    for {
      _ <- Either.cond(
        ids.forall(_.getModType == idType),
        {},
        ToplRpcErrors.unsupportedOperation(s"The requested id's type is not an id type for $modifierTypeName")
      )
    } yield ids
  }

  private[handlers] def checkBlocksFoundWithIds(
    ids:          List[ModifierId],
    blocksOption: List[Option[Block]],
    sizeLimit:    Int
  ): Either[RpcError, List[Block]] =
    for {
      _ <- Either.cond(
        ids.size <= sizeLimit,
        {},
        ToplRpcErrors.unsupportedOperation("Number of ids given exceeded blockRetrievalLimit")
      )
      blocks <- Either.cond(
        blocksOption.forall(_.nonEmpty),
        blocksOption.map(_.get),
        ToplRpcErrors.NoBlockWithId
      )
    } yield blocks

  private[handlers] def checkTxFoundWithIds[T](
    ids:       List[ModifierId],
    txOption:  List[Option[T]],
    sizeLimit: Int
  ): Either[RpcError, List[T]] =
    for {
      _ <- Either.cond(
        ids.size <= sizeLimit,
        {},
        ToplRpcErrors.unsupportedOperation("Number of ids given exceeded txRetrievalLimit")
      )
      txs <- Either.cond(
        txOption.forall(_.nonEmpty),
        txOption.map(_.get),
        ToplRpcErrors.NoTransactionWithId
      )
    } yield txs

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
}
