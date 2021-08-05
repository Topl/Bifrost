package co.topl.akkahttprpc

import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import cats.implicits._
import co.topl.akkahttprpc.RpcErrorCodecs._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

import java.util.UUID
import scala.concurrent.ExecutionContext
import scala.concurrent.duration._
import scala.language.implicitConversions
import scala.util.Try

trait RpcDirectives {

  import RpcEncoders._

  def rejectionHandler(implicit throwableEncoder: Encoder[ThrowableData]): RejectionHandler =
    RejectionHandler
      .newBuilder()
      .handleAll[RpcErrorRejection](rejections =>
        complete(rpcErrorToFailureResponseUnknownContext(rejections.last.rpcError).asJson)
      )
      .result()

  private[akkahttprpc] def recoverToRpcError(implicit
    throwableEncoder: Encoder[ThrowableData]
  ): PartialFunction[Throwable, CustomError] = { case e =>
    CustomError.fromThrowable(e)
  }

  implicit def rpcErrorAsRejection(rpcError: RpcError): Rejection =
    RpcErrorRejection(rpcError)

  def rpcRoute[RpcParams: Decoder, SuccessResult: Encoder](
    method:                    String,
    handler:                   Rpc.ServerHandler[RpcParams, SuccessResult]
  )(implicit throwableEncoder: Encoder[ThrowableData]): Route =
    rpcContextWithParams[RpcParams](method).tapply { case (context, params) =>
      implicit val c: RpcContext = context
      extractExecutionContext { implicit ec: ExecutionContext =>
        onComplete(
          handler(params)
            .map(r => SuccessRpcResponse(context.id, context.jsonrpc, r.asJson))
            .value
        )(_.toEither.leftMap(recoverToRpcError).flatMap(identity).fold(completeRpc, completeRpc))
      }
    }

  def rpcContextWithParams[RpcParams: Decoder](
    method:                    String
  )(implicit throwableEncoder: Encoder[ThrowableData]): Directive[(RpcContext, RpcParams)] =
    rpcContext.flatMap(ctx => filterRpcMethod(method)(ctx).tmap(_ => ctx)).flatMap { implicit ctx =>
      rpcParameters[RpcParams].map((ctx, _))
    }

  def filterRpcMethod(method: String)(implicit rpcContext: RpcContext): Directive0 =
    if (rpcContext.method == method) pass else reject(MethodNotFoundError(rpcContext.method))

  def completeRpc(rawRpcResponse: RpcResponse): StandardRoute =
    complete(rawRpcResponse)

  def completeRpc(
    error:               RpcError
  )(implicit rpcContext: RpcContext, throwableEncoder: Encoder[ThrowableData]): StandardRoute =
    completeRpc(rpcErrorToFailureResponse(rpcContext, error))

  def rpcParameters[RpcParams: Decoder](implicit
    context:          RpcContext,
    throwableEncoder: Encoder[ThrowableData]
  ): Directive1[RpcParams] =
    Try(
      context.params
        .as[RpcParams]
        .leftMap(InvalidParametersError(_))
    ).toEither
      .leftMap(throwable => InvalidParametersError(DecodingFailure.fromThrowable(throwable, Nil)))
      .flatten
      .fold(completeRpc(_).toDirective, provide)

  def rpcContext: Directive1[RpcContext] =
    post
      .tflatMap(_ =>
        extractStrictEntity(5.seconds)
          .map(_.data.utf8String)
          .flatMap(
            parser
              .parse(_)
              .leftMap(_ => ParseError: RpcError)
              .filterOrElse(_.isObject, ParseError)
              .flatMap(_.as[RpcContext].leftMap(InvalidRequestError.apply))
              .fold(reject(_), provide)
          )
      )

  private def rpcErrorToFailureResponseUnknownContext(
    error:                     RpcError
  )(implicit throwableEncoder: Encoder[ThrowableData]): FailureRpcResponse =
    FailureRpcResponse(
      UUID.randomUUID().toString,
      "2.0",
      FailureRpcResponse.Error(error.code, error.message, Some(encodeRpcData(error)))
    )

  private def rpcErrorToFailureResponse(request: RpcContext, error: RpcError)(implicit
    throwableEncoder:                            Encoder[ThrowableData]
  ): FailureRpcResponse =
    FailureRpcResponse(
      request.id,
      request.jsonrpc,
      FailureRpcResponse.Error(error.code, error.message, Some(encodeRpcData(error)))
    )

}

object RpcDirectives extends RpcDirectives

object RpcEncoders {

  implicit val decodeRpcContext: Decoder[RpcContext] =
    deriveDecoder[RpcContext]
      .map(context =>
        context.copy(params =
          context.params.arrayOrObject[Json](context.params, _.headOption.getOrElse(Json.Null), _ => context.params)
        )
      )

  implicit val encodeRpcContext: Encoder[RpcContext] =
    deriveEncoder[RpcContext]

  implicit val encodeFailureRpcResponseError: Codec[FailureRpcResponse.Error] =
    deriveCodec[FailureRpcResponse.Error]

  implicit val encodeSuccessResponse: Codec[SuccessRpcResponse] =
    deriveCodec[SuccessRpcResponse]

  implicit val encodeFailureResponse: Codec[FailureRpcResponse] =
    deriveCodec[FailureRpcResponse]

  implicit val encodeRawRpcResponse: Encoder[RpcResponse] =
    Encoder.instance {
      case s: SuccessRpcResponse => s.asJson
      case f: FailureRpcResponse => f.asJson
    }

  implicit val decodeRawRpcResponse: Decoder[RpcResponse] =
    (encodeSuccessResponse: Decoder[SuccessRpcResponse]).widen
      .or((encodeFailureResponse: Decoder[FailureRpcResponse]).widen)
}

case class RpcErrorRejection(rpcError: RpcError) extends Rejection
