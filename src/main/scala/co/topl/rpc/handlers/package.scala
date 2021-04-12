package co.topl.rpc

import co.topl.akkahttprpc.RpcError
import co.topl.attestation.Address
import co.topl.nodeView.state.State

package object handlers {

  private[handlers] def checkAddresses(keys: List[Address], state: State): Either[RpcError, List[Address]] =
    for {
      _ <- Either.cond(state.hasTBR, {}, ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node"))
      _ <- Either.cond(
        state.nodeKeys.forall(_.intersect(keys.toSet).nonEmpty),
        {},
        ToplRpcErrors.unsupportedOperation("TokenBoxRegistry not defined for node")
      )
    } yield keys

}
