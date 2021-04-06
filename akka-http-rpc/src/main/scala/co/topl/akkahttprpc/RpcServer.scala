package co.topl.akkahttprpc

import akka.http.scaladsl.server.Route
import co.topl.akkahttprpc.RpcDirectives.rpcRoute
import io.circe.{Decoder, Encoder}

class RpcServer[Params, SuccessResponse](val rpc: Rpc[Params, SuccessResponse]) extends AnyVal {

  def serve(handler:        rpc.ServerHandler)(implicit
    paramsDecoder:          Decoder[Params],
    successResponseEncoder: Encoder[SuccessResponse],
    throwableEncoder:       Encoder[ThrowableData]
  ): Route =
    rpcRoute[Params, SuccessResponse](rpc.method, handler)

}
