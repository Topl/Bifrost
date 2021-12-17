package co.topl.akkahttprpc

import akka.actor.ActorSystem
import akka.http.scaladsl.Http
import akka.http.scaladsl.model._
import cats.data.EitherT
import cats.implicits._
import co.topl.akkahttprpc.RpcEncoders._
import co.topl.akkahttprpc.RpcErrorCodecs._
import io.circe._
import io.circe.syntax._

import java.util.UUID
import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}
import scala.util.Success

class RpcClient[Params, SuccessResponse](val rpc: Rpc[Params, SuccessResponse]) extends AnyVal {

  def apply(params:         Params)(implicit
    paramsEncoder:          Encoder[Params],
    successResponseDecoder: Decoder[SuccessResponse],
    requestModifier:        RequestModifier,
    system:                 ActorSystem,
    ec:                     ExecutionContext
  ): EitherT[Future, RpcClientFailure, SuccessResponse] = call.apply(params)

  def call(implicit
    paramsEncoder:          Encoder[Params],
    successResponseDecoder: Decoder[SuccessResponse],
    requestModifier:        RequestModifier,
    system:                 ActorSystem,
    ec:                     ExecutionContext
  ): rpc.ClientHandler =
    params => sendRequest(asRequest(params)(paramsEncoder, requestModifier)).flatMap(handleResponse(_))

  private[akkahttprpc] def asRequest(
    params:                 Params
  )(implicit paramsEncoder: Encoder[Params], requestModifier: RequestModifier): HttpRequest =
    requestModifier.f(
      HttpRequest(
        HttpMethods.POST,
        entity = HttpEntity.apply(
          ContentTypes.`application/json`,
          RpcContext(
            id = UUID.randomUUID().toString,
            jsonrpc = "2.0",
            method = rpc.methods.head,
            params = List(params).asJson
          ).asJson.toString()
        )
      )
    )

  private[akkahttprpc] def sendRequest(
    request:         HttpRequest
  )(implicit system: ActorSystem, ec: ExecutionContext): EitherT[Future, RpcClientFailure, HttpResponse] =
    EitherT(
      Http()
        .singleRequest(request)
        .transform(t => Success(t.toEither))
    )
      .leftMap(HttpExceptionFailure.apply)

  private[akkahttprpc] def handleResponse(r: HttpResponse)(implicit
    successResponseDecoder:                  Decoder[SuccessResponse],
    system:                                  ActorSystem,
    ec:                                      ExecutionContext
  ): EitherT[Future, RpcClientFailure, SuccessResponse] =
    r.status match {
      case StatusCodes.OK =>
        EitherT(
          r.entity
            .toStrict(5.seconds)
            .map(_.data.utf8String)
            .map(parser.parse)
        )
          .subflatMap(_.as[RpcResponse])
          .leftMap(_ => UnexpectedResponseFailure(r): RpcClientFailure)
          .subflatMap[RpcClientFailure, SuccessResponse] {
            case SuccessRpcResponse(_, _, result) =>
              result.as[SuccessResponse].leftMap(_ => UnexpectedResponseFailure(r): RpcClientFailure)
            case FailureRpcResponse(_, _, error) =>
              error.asJson
                .as[RpcError]
                .leftMap(_ => UnexpectedResponseFailure(r): RpcClientFailure)
                .flatMap(e => (RpcErrorFailure(e): RpcClientFailure).asLeft)
          }
      case _ =>
        EitherT.leftT[Future, SuccessResponse](UnexpectedResponseFailure(r): RpcClientFailure)
    }

}

object RpcClient {}

sealed abstract class RpcClientFailure
case class RpcErrorFailure(rpcError: RpcError) extends RpcClientFailure
case class HttpExceptionFailure(throwable: Throwable) extends RpcClientFailure
case class UnexpectedResponseFailure(response: HttpResponse) extends RpcClientFailure

case class RequestModifier(f: HttpRequest => HttpRequest)
