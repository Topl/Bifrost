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
          .leftMap(InvalidParametersError.apply)
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

object RpcHandler {

  def apply[Params: Decoder, ErrorResult: RpcErrorEncoder, SuccessResult: Encoder](
    f: Params => EitherT[Future, ErrorResult, SuccessResult]
  ): RpcHandler[Params, ErrorResult, SuccessResult] =
    new RpcHandler[Params, ErrorResult, SuccessResult] {
      def apply(params: Params): EitherT[Future, ErrorResult, SuccessResult] = f(params)
    }
}

trait RpcErrorEncoder[ErrorResult] {
  def toRpcError(outError: ErrorResult): RpcError[_]
}
