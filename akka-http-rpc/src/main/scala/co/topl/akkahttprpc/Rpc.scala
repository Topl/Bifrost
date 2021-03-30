package co.topl.akkahttprpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server.{Directive1, Route}
import cats.data.{EitherT, NonEmptyMap}
import cats.implicits._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder}

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

  def route(handlers: NonEmptyMap[String, RpcHandler[_, _, _]]): Route =
    rawRpc { request =>
      extractExecutionContext { implicit ec: ExecutionContext =>
        complete {
          findMethod(handlers, request.method)
            .leftMap(rpcErrorToFailureResponse(request, _))
            .flatMap(_.apply(request))
            .map(_.asJson)
            .leftMap(_.asJson)
            .merge
            .recover(recoverToRpcError.andThen(rpcErrorToFailureResponse(request, _).asJson))
        }
      }
    }

  private[akkahttprpc] def findMethod(handlers: NonEmptyMap[String, RpcHandler[_, _, _]], method: String)(implicit
    ec:                                         ExecutionContext
  ) =
    EitherT
      .fromOption[Future](handlers(method), MethodNotFoundError(method))

  private[akkahttprpc] def rawRpc: Directive1[RawRpcRequest] =
    post.tflatMap(_ => entity(as[RawRpcRequest]))

  private def recoverToRpcError: PartialFunction[Throwable, ServerError[Throwable]] = { case e =>
    ServerError.fromThrowable(-32099, "Unknown server error", e)
  }

}
