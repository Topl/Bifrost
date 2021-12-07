package co.topl.akkahttprpc

import cats.data.EitherT

import scala.concurrent.Future

case class Rpc[P, SR](method: List[String]) {
  type Params = P
  type SuccessResponse = SR
  type ClientHandler = Rpc.ClientHandler[Params, SuccessResponse]
  type ServerHandler = Rpc.ServerHandler[Params, SuccessResponse]
}

object Rpc {
  type ClientHandler[-Params, SuccessResponse] = Params => EitherT[Future, RpcClientFailure, SuccessResponse]
  type ServerHandler[-Params, SuccessResponse] = Params => EitherT[Future, RpcError, SuccessResponse]
}
