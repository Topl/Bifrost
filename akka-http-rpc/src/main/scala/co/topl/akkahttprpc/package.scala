package co.topl

package object akkahttprpc {

  def rpcErrorToFailureResponse(request: RawRpcRequest, error: RpcError[_]): FailureRpcResponse =
    FailureRpcResponse(
      request.id,
      request.jsonrpc,
      FailureRpcResponse.Error(error.code, error.message, error.encodedData)
    )
}
