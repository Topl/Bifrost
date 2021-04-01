package co.topl.akkahttprpc

import akka.http.scaladsl.server.Rejection
import cats.data.NonEmptyChain
import cats.implicits._
import co.topl.akkahttprpc.JsonFailureSupport.decodingFailureEncoder
import io.circe.syntax._
import io.circe.{DecodingFailure, Encoder, Json}

sealed abstract class RpcError[Data: Encoder] {
  def code: Int
  def message: String
  def data: Option[Data]

  def encodedData: Option[Json] = data.map(_.asJson)
}

case object ParseError extends RpcError[None.type] {
  override val code: Int = -32700

  override val message: String = "Invalid JSON"

  override val data: Option[None.type] = None
}

case class InvalidRequestError(decodingFailure: DecodingFailure) extends RpcError[DecodingFailure] {

  override val code: Int = -32600

  override val message: String = "Invalid RPC Request Format"

  override def data: Option[DecodingFailure] = Some(decodingFailure)
}

case class MethodNotFoundError(method: String) extends RpcError[MethodNotFoundError.Data] {

  override val code: Int = -32601

  override val message: String = "RPC Method Not Found"

  override def data: Option[MethodNotFoundError.Data] = Some(MethodNotFoundError.Data(method))
}

object MethodNotFoundError {
  case class Data(method: String)
  import io.circe.generic.semiauto._
  implicit val dataEncoder: Encoder[Data] = deriveEncoder[Data]
}

case class InvalidParametersError(parameterErrors: NonEmptyChain[InvalidParametersError.Error])
    extends RpcError[InvalidParametersError.Data] {

  override val code: Int = -32602

  override val message: String = "Invalid method parameter(s)"

  override def data: Option[InvalidParametersError.Data] =
    Some(InvalidParametersError.Data(parameterErrors))
}

object InvalidParametersError {

  def apply(decodingFailure: DecodingFailure): InvalidParametersError =
    InvalidParametersError(
      NonEmptyChain.one(Error(decodingFailure.history.map(_.show), decodingFailure.message, None))
    )

  case class Error(path: List[String], message: String, data: Option[Json])
  case class Data(errors: NonEmptyChain[Error])

  import io.circe.generic.semiauto._
  implicit val errorEncoder: Encoder[Error] = deriveEncoder[Error]
  implicit val dataEncoder: Encoder[Data] = deriveEncoder[Data]
}

case class InternalJsonRpcError(reason: String, throwable: Option[Throwable])
    extends RpcError[InternalJsonRpcError.Data] {
  override val code: Int = -32603

  override val message: String = "Internal JSON-RPC Error"

  override def data: Option[InternalJsonRpcError.Data] =
    Some(InternalJsonRpcError.Data(reason, throwable))
}

object InternalJsonRpcError {
  case class Data(reason: String, throwable: Option[Throwable])
  import JsonFailureSupport.throwableEncoder

  implicit val encoder: Encoder[Data] =
    Encoder.forProduct2("reason", "throwable")(data => (data.reason, data.throwable))
}

case class CustomError[Data: Encoder](code: Int, message: String, data: Option[Data]) extends RpcError[Data]

object CustomError {
  import JsonFailureSupport.throwableEncoder

  def fromThrowable(code: Int, message: String, throwable: Throwable): CustomError[Throwable] =
    CustomError[Throwable](code, message, Some(throwable))
}

object JsonFailureSupport {

  implicit val decodingFailureEncoder: Encoder[DecodingFailure] =
    decodingFailure =>
      Map(
        "path" -> decodingFailure.history.map(_.show).asJson
      ).asJson

  implicit val throwableEncoder: Encoder[Throwable] =
    throwable =>
      Map(
        "message" -> Option(throwable.getMessage).asJson,
        // TODO: Verbose API Settings only?
        "stackTrace" -> throwable.getStackTrace.map(_.toString).asJson
      ).asJson
}
