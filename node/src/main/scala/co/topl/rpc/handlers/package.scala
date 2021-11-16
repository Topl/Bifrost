package co.topl.rpc

import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
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

  private[handlers] def checkHeightRange(
    bestBlockHeight: Long,
    startHeight:     Long,
    endHeight:       Long
  ): Either[RpcError, (Long, Long)] =
    for {
      _ <- Either.cond(
        startHeight >= 1 && endHeight >= startHeight && bestBlockHeight >= startHeight,
        {},
        ToplRpcErrors.InvalidHeightRange)
    } yield (startHeight, endHeight)

}
