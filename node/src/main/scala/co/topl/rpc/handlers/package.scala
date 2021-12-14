package co.topl.rpc

import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.nodeView.state.StateReader
import co.topl.rpc.ToplRpc.NodeView.ConfirmationStatus.TxStatus

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

  private[handlers] def checkBlocksFoundWithIds(
    ids:          List[ModifierId],
    blocksOption: List[Option[Block]],
    sizeLimit:    Int
  ): Either[RpcError, ToplRpc.NodeView.BlocksByIds.Response] =
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

  private[handlers] def checkHeightRange(
    bestBlockHeight: Long,
    startHeight:     Long,
    endHeight:       Long,
    sizeLimit:       Int
  ): Either[RpcError, (Long, Long)] =
    for {
      _ <- Either.cond(
        startHeight >= 1 && endHeight >= startHeight && bestBlockHeight >= startHeight && bestBlockHeight >= endHeight,
        {},
        ToplRpcErrors.InvalidHeightRange
      )
      _ <- Either.cond(
        (endHeight - startHeight + 1) <= sizeLimit,
        {},
        ToplRpcErrors.unsupportedOperation("Height range exceeded blockRetrievalLimit")
      )
    } yield (startHeight, endHeight)

  private[handlers] def checkTxIds(
    txStatusOption: List[Option[(ModifierId, TxStatus)]]
  ): Either[RpcError, ToplRpc.NodeView.ConfirmationStatus.Response] =
    for {
      txStatus <- Either.cond(
        txStatusOption.forall(_.nonEmpty),
        txStatusOption.map(_.get).toMap,
        ToplRpcErrors.NoTransactionWithId
      )
    } yield txStatus

}
