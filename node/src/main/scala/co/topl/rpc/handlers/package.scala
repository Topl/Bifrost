package co.topl.rpc

import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.ReadableNodeView
import co.topl.nodeView.history.HistoryReader
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

  private[handlers] def blocksByIdsResponse(
    ids:       List[ModifierId],
    sizeLimit: Int,
    history:   HistoryReader[Block, BifrostSyncInfo]
  ): Either[RpcError, ToplRpc.NodeView.BlocksByIds.Response] = {
    val blocksOption = ids.map(history.modifierById)
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
  }

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
        (endHeight - startHeight + 1) <= sizeLimit,
        {},
        ToplRpcErrors.unsupportedOperation("Height range exceeded blockRetrievalLimit")
      )
    } yield (startHeight, endHeight)

  private[handlers] def checkTxIds(
    txIds: List[ModifierId],
    view:  ReadableNodeView
  ): Either[RpcError, ToplRpc.NodeView.ConfirmationStatus.Response] = {
    val txStatusOption: List[Option[(ModifierId, TxStatus)]] = txIds.map { id =>
      val mempoolStatus = view.memPool.modifierById(id)
      val historyStatus = view.history.transactionById(id)
      val bestBlockHeight = view.history.height
      (mempoolStatus, historyStatus) match {
        case (_, Some((tx, _, height))) =>
          Some(tx.id -> TxStatus("Confirmed", bestBlockHeight - height))
        case (Some(tx), None) =>
          Some(tx.id -> TxStatus("Unconfirmed", -1))
        case (None, None) =>
          None
      }
    }

    for {
      txStatus <- Either.cond(
        txStatusOption.forall(_.nonEmpty),
        txStatusOption.map(_.get).toMap,
        ToplRpcErrors.NoTransactionWithId
      )
    } yield txStatus
  }

}
