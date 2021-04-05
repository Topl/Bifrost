package co.topl.akkahttprpc

import akka.http.scaladsl.model.HttpRequest
import akka.http.scaladsl.server.Route
import cats.data.EitherT
import co.topl.akkahttprpc.RpcDirectives._
import io.circe.{Decoder, Encoder}

import scala.concurrent.Future

case class Rpc[Params, SuccessResponse](method: String) {

  type Handler = Rpc.Handler[Params, SuccessResponse]

  def serve(handler:              Handler)(implicit
                                           paramsDecoder:          Decoder[Params],
                                           successResponseEncoder: Encoder[SuccessResponse],
                                           throwableEncoder:       Encoder[ThrowableData]
  ): Route =
    rpcRoute[Params, SuccessResponse](method, handler)

  def call(implicit
    paramsEncoder:          Encoder[Params],
    successResponseDecoder: Decoder[SuccessResponse],
    requestModifier:        HttpRequest => HttpRequest = identity
  ): Handler =
    ???

}

object Rpc {
  type Handler[Params, Response] = Params => EitherT[Future, RpcError[_], Response]
}
