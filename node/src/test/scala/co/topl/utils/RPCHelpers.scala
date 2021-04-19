package co.topl.utils

import akka.util.ByteString

trait RPCHelpers {

  def formRequest(method: String, params: String): ByteString = {
    ByteString(
      s"""{
         |  "jsonrpc": "2.0",
         |  "id": "1",
         |  "method": "$method",
         |  "params": [{$params}]
         |}
         |""".stripMargin)
  }
}
