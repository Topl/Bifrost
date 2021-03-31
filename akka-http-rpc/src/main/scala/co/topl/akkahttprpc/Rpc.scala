package co.topl.akkahttprpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, MalformedRequestContentRejection, RejectionHandler, Route}
import cats.data.{EitherT, NonEmptyMap}
import cats.implicits._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json}

import scala.concurrent.{ExecutionContext, Future}

object Rpc {

  implicit val decodeRawRpcRequest: Decoder[RawRpcRequest] =
    deriveDecoder[RawRpcRequest]

  implicit val encodeSuccessResponse: Encoder[SuccessRpcResponse] =
    deriveEncoder[SuccessRpcResponse]

  implicit val encodeFailureRpcResponseError: Encoder[FailureRpcResponse.Error] =
    deriveEncoder[FailureRpcResponse.Error]

  implicit val encodeFailureResponse: Encoder[FailureRpcResponse] =
    deriveEncoder[FailureRpcResponse]

  implicit val rejectionHandler: RejectionHandler =
    RejectionHandler
      .newBuilder()
      .handle { case e: RpcError[_] =>
        complete(rpcErrorToFailureResponse(e))
      }
      .result()

  def route(handlers: NonEmptyMap[String, RpcHandler[_, _, _]]): Route =
      rawRpc { request =>
        extractExecutionContext { implicit ec: ExecutionContext =>
          complete {
            findMethod(handlers, request)
              .flatMap(_.apply(request))
              .fold(_.asJson, _.asJson)
              .recover(recoverToRpcError.andThen(rpcErrorToFailureResponse(request, _).asJson))
          }
        }
      }

  private[akkahttprpc] def findMethod(handlers: NonEmptyMap[String, RpcHandler[_, _, _]], request: RawRpcRequest)(implicit
    ec:                                         ExecutionContext
  ) =
    EitherT
      .fromOption[Future](handlers(request.method), MethodNotFoundError(request.method))
      .leftMap(rpcErrorToFailureResponse(request, _))

  private[akkahttprpc] def rawRpc: Directive1[RawRpcRequest] =
    post
      .tflatMap(_ =>
        entity(as[Json])
          .and(
            mapRejections(_.map {
              case _: MalformedRequestContentRejection => ParseError
              case r                                   => r
            })
          )
          .flatMap(_.as[RawRpcRequest].fold(decodingFailure => reject(InvalidRequestError(decodingFailure)), provide))
      )

  private[akkahttprpc] def recoverToRpcError: PartialFunction[Throwable, CustomError[Throwable]] = { case e =>
    CustomError.fromThrowable(-32099, "Unknown server error", e)
  }

}
