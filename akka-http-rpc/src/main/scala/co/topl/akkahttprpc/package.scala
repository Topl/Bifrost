package co.topl

import java.util.UUID

package object akkahttprpc {

  def rpcErrorToFailureResponse(error: RpcError[_]): FailureRpcResponse =
    FailureRpcResponse(
      UUID.randomUUID().toString,
      "2.0",
      FailureRpcResponse.Error(error.code, error.message, error.encodedData)
    )

  def rpcErrorToFailureResponse(request: RpcContext, error: RpcError[_]): FailureRpcResponse =
    FailureRpcResponse(
      request.id,
      request.jsonrpc,
      FailureRpcResponse.Error(error.code, error.message, error.encodedData)
    )
}
