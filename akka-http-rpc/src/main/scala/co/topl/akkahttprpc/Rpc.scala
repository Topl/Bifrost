package co.topl.akkahttprpc

import akka.http.scaladsl.marshalling.Marshaller
import akka.http.scaladsl.server.Directives._
import akka.http.scaladsl.server._
import cats.data.EitherT
import cats.implicits._
import de.heikoseeberger.akkahttpcirce.ErrorAccumulatingCirceSupport._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, Json, _}

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.language.implicitConversions
import scala.util.{Failure, Success}

object RpcEncoders {

  implicit val decodeRawRpcRequest: Decoder[RpcContext] =
    deriveDecoder[RpcContext]

  implicit val encodeFailureRpcResponseError: Encoder[FailureRpcResponse.Error] =
    deriveEncoder[FailureRpcResponse.Error]

  implicit val encodeSuccessResponse: Encoder[SuccessRpcResponse] =
    deriveEncoder[SuccessRpcResponse]

  implicit val encodeFailureResponse: Encoder[FailureRpcResponse] =
    deriveEncoder[FailureRpcResponse]

  implicit val encodeRawRpcResponse: Encoder[RawRpcResponse] =
    Encoder.instance {
      case s: SuccessRpcResponse => s.asJson
      case f: FailureRpcResponse => f.asJson
    }
}

trait RpcDirectives {

  import RpcEncoders._

  type RpcResponseMarshaller[T] = Marshaller[T, RawRpcResponse]

  type JsonMarshaller[T] = Marshaller[T, Json]

  implicit val rejectionHandler: RejectionHandler =
    RejectionHandler
      .newBuilder()
      .handle { case RpcErrorRejection(e: RpcError[_]) =>
        complete(rpcErrorToFailureResponse(e).asJson)
      }
      .result()

  private[akkahttprpc] def recoverToRpcError: PartialFunction[Throwable, CustomError[Throwable]] = { case e =>
    CustomError.fromThrowable(-32099, "Unknown server error", e)
  }

  implicit def encoderAsJsonMarshaller[T](t: Encoder[T]): JsonMarshaller[T] =
    Marshaller.opaque(t.apply)

  implicit def rpcErrorAsRejection(rpcError: RpcError[_]): Rejection =
    RpcErrorRejection(rpcError)

  def rpc[RpcParams: Decoder, ErrorResult: RpcErrorEncoder, SuccessResult: Encoder](
    method:  String,
    handler: RpcParams => EitherT[Future, ErrorResult, SuccessResult]
  ): Route =
    extractRpc[RpcParams](method).tapply { case (context, params) =>
      implicit val c: RpcContext = context
      extractExecutionContext { implicit ec: ExecutionContext =>
        onComplete(
          handler(params)
            .map(r => SuccessRpcResponse(context.id, context.jsonrpc, r.asJson))
            .leftMap(r => implicitly[RpcErrorEncoder[ErrorResult]].toRpcError(r))
            .value
        ) {
          case Success(Right(success)) => completeRpc(success)
          case Success(Left(reason))   => reject(RpcErrorRejection(reason))
          case Failure(exception)      => reject(RpcErrorRejection(recoverToRpcError(exception)))
        }
      }
    }

  def extractRpc[RpcParams: Decoder](method: String): Directive[(RpcContext, RpcParams)] =
    extractRpcContext.flatMap(ctx => extractRpcMethod(method)(ctx).tmap(_ => ctx)).flatMap { implicit ctx =>
      extractDecodedRpcParameters[RpcParams].map((ctx, _))
    }

  def extractRpcMethod(method: String)(implicit rpcContext: RpcContext): Directive0 =
    if (rpcContext.method == method) pass else reject(MethodNotFoundError(rpcContext.method))

  def completeRpc(rawRpcResponse: RawRpcResponse): StandardRoute =
    complete(rawRpcResponse)

  def completeRpc(error: RpcError[_])(implicit rpcContext: RpcContext): StandardRoute =
    completeRpc(rpcErrorToFailureResponse(rpcContext, error))

  def extractDecodedRpcParameters[RpcParams: Decoder](implicit context: RpcContext): Directive1[RpcParams] =
    context.params
      .as[RpcParams]
      .leftMap(InvalidParametersError(_): Rejection)
      .fold(reject(_), provide)

  def extractRpcContext: Directive1[RpcContext] =
    post
      .tflatMap(_ =>
        extractStrictEntity(5.seconds)
          .map(_.data.utf8String)
          .flatMap(parser.parse(_).fold(_ => reject(ParseError).toDirective[Tuple1[Json]], provide))
          .filter(_.isObject, ParseError)
          .flatMap(_.as[RpcContext].leftMap(InvalidRequestError.apply).fold(reject(_), provide))
      )
}

case class RpcErrorRejection(rpcError: RpcError[_]) extends Rejection
