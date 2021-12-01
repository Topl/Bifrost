package co.topl.rpc

import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
import co.topl.modifier.ModifierId
import co.topl.modifier.block.Block
import co.topl.network.message.BifrostSyncInfo
import co.topl.nodeView.history.HistoryReader
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

  private[handlers] def blocksByIdsResponse(
    ids:     List[ModifierId],
    history: HistoryReader[Block, BifrostSyncInfo]
  ): Either[RpcError, ToplRpc.NodeView.BlocksByIds.Response] = {
    val blocksOption = ids.map(history.modifierById)
    for {
      blocks <- Either.cond(
        blocksOption.forall(_.nonEmpty),
        blocksOption.map(_.get),
        ToplRpcErrors.unsupportedOperation("No corresponding block found for id")
      )
    } yield blocks
  }

  private[handlers] def checkHeightRange(
    bestBlockHeight: Long,
    startHeight:     Long,
    endHeight:       Long
  ): Either[RpcError, (Long, Long)] =
    for {
      _ <- Either.cond(
        startHeight >= 1 && endHeight >= startHeight && bestBlockHeight >= startHeight,
        {},
        ToplRpcErrors.InvalidHeightRange
      )
    } yield (startHeight, endHeight)

}
