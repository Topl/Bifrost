package co.topl.akkahttprpc

import io.circe.Json

sealed trait RpcResponse {
  def id: String
  def jsonrpc: String
}

case class SuccessRpcResponse(id: String, jsonrpc: String, result: Json) extends RpcResponse
case class FailureRpcResponse(id: String, jsonrpc: String, error: FailureRpcResponse.Error) extends RpcResponse

object FailureRpcResponse {
  case class Error(code: Int, message: String, data: Option[Json])
}
