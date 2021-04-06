package co.topl.akkahttprpc

import cats.data.{EitherT, Kleisli}

import scala.concurrent.Future

case class Rpc[P, SR](method: String) {
  type Params = P
  type SuccessResponse = SR
  type ClientHandler = Rpc.ClientHandler[Params, SuccessResponse]
  type ServerHandler = Rpc.ServerHandler[Params, SuccessResponse]

}

object Rpc {
  type ClientResponse[SuccessResponse] = EitherT[Future, RpcClientFailure, SuccessResponse]
  type ClientHandler[-Params, SuccessResponse] = Kleisli[ClientResponse, Params, SuccessResponse]

  type ServerResponse[SuccessResponse] = EitherT[Future, RpcError[_], SuccessResponse]
  type ServerHandler[-Params, SuccessResponse] = Kleisli[ServerResponse, Params, SuccessResponse]
}
