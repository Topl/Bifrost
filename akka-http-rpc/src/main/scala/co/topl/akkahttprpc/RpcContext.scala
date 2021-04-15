package co.topl.akkahttprpc

import io.circe.Json

case class RpcContext(id: String, jsonrpc: String, method: String, params: Json)
