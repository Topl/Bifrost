package co.topl.akkahttprpc

import io.circe.Json

case class RawRpcRequest(id: String, jsonrpc: String, method: String, params: Json)

sealed abstract class RawRpcResponse {
  def id: String
  def jsonrpc: String
}

case class SuccessRpcResponse(id: String, jsonrpc: String, result: Json) extends RawRpcResponse
case class FailureRpcResponse(id: String, jsonrpc: String, error: FailureRpcResponse.Error) extends RawRpcResponse

object FailureRpcResponse {
  case class Error(code: Int, message: String, data: Option[Json])
}
