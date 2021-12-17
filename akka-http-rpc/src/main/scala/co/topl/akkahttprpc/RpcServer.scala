package co.topl.akkahttprpc

import akka.http.scaladsl.server.Route
import cats.kernel.Semigroup
import co.topl.akkahttprpc.RpcDirectives.{rpcRoute, rpcRoutes}
import io.circe.{Decoder, Encoder}

class RpcServer[Params, SuccessResponse](val rpc: Rpc[Params, SuccessResponse]) extends AnyVal {

  /**
   * Constructs a Route which serves a single RPC handler for the first rpc method name
   */
  def serve(handler:        rpc.ServerHandler)(implicit
    paramsDecoder:          Decoder[Params],
    successResponseEncoder: Encoder[SuccessResponse],
    throwableEncoder:       Encoder[ThrowableData]
  ): Route =
    rpcRoute[Params, SuccessResponse](rpc.methods.head, handler)

}

object RpcServer {

  case class Builder(handlers: Map[String, Builder.BuilderHandler[_, _]]) {
    def route: Route = rpcRoutes(this)

    def append[Params, SuccessResponse](rpc: Rpc[Params, SuccessResponse])(handler: rpc.ServerHandler)(implicit
      paramsDecoder:                         Decoder[Params],
      successResponseEncoder:                Encoder[SuccessResponse],
      throwableEncoder:                      Encoder[ThrowableData]
    ): Builder =
      copy(handlers =
        handlers ++ rpc.methods.map(
          (_, Builder.BuilderHandler(handler, paramsDecoder, successResponseEncoder, throwableEncoder))
        )
      )
  }

  /**
   * A "Builder" pattern for constructing an akka-http Route using many RPC handlers
   *
   * @example
   *    RpcServer.Builder.empty
   *      .append(rpc)(handler)
   *      .append(rpc2)(handler2)
   *      .route
   */
  object Builder {
    val empty: Builder = Builder(Map.empty)

    private[RpcServer] case class BuilderHandler[P, R](
      handler:                Rpc.ServerHandler[P, R],
      paramsDecoder:          Decoder[P],
      successResponseEncoder: Encoder[R],
      throwableEncoder:       Encoder[ThrowableData]
    ) {
      type Params = P
      type Response = R
    }

    implicit val builderSemigroup: Semigroup[Builder] = (a, b) => Builder(a.handlers ++ b.handlers)
  }
}
