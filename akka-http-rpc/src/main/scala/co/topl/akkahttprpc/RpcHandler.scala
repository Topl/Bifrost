package co.topl.akkahttprpc

import cats.data.EitherT
import cats.implicits._
import io.circe._
import io.circe.syntax._

import scala.concurrent.{ExecutionContext, Future}

abstract class RpcHandler[Params: Decoder, ErrorResult: RpcErrorEncoder, SuccessResult: Encoder]
    extends (Params => EitherT[Future, ErrorResult, SuccessResult]) {

  def apply(
    rawRpcRequest: RawRpcRequest
  )(implicit ec:   ExecutionContext): EitherT[Future, FailureRpcResponse, SuccessRpcResponse] =
    EitherT
      .fromEither[Future](
        implicitly[Decoder[Params]]
          .decodeJson(rawRpcRequest.params)
          .leftMap(InvalidRequestError)
      )
      .flatMap(apply(_).leftMap(implicitly[RpcErrorEncoder[ErrorResult]].toRpcError(_)))
      .leftMap(e =>
        FailureRpcResponse(
          rawRpcRequest.id,
          rawRpcRequest.jsonrpc,
          FailureRpcResponse.Error(e.code, e.message, e.encodedData)
        )
      )
      .map(success => SuccessRpcResponse(rawRpcRequest.id, rawRpcRequest.jsonrpc, success.asJson))
}

trait RpcErrorEncoder[ErrorResult] {
  def toRpcError(outError: ErrorResult): RpcError[_]
}
