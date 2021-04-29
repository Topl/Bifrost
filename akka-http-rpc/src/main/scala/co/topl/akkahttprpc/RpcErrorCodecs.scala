package co.topl.akkahttprpc

import cats.data.NonEmptyChain
import cats.implicits._
import io.circe.generic.semiauto._
import io.circe.syntax._
import io.circe.{Decoder, Encoder, _}

trait RpcErrorCodecs {

  val encodeNoData: Encoder[RpcError] =
    a => Map("code" -> a.code.asJson, "message" -> a.message.asJson).asJson

  private def encodeWithData[Data: Encoder](e: RpcError, data: Data): Json =
    Map("code" -> e.code.asJson, "message" -> e.message.asJson, "data" -> data.asJson).asJson

  implicit val parseErrorCodec: Codec[ParseError.type] =
    Codec.from(
      _ => ParseError.asRight,
      encodeNoData.apply
    )

  implicit val decodingFailureCodec: Codec[DecodingFailure] =
    Codec.from(
      c => c.downField("message").as[String].map(DecodingFailure(_, Nil)), // TODO: Decode path
      a =>
        Map(
          "message" -> a.message.asJson,
          "path"    -> CursorOp.opsToPath(a.history).asJson
        ).asJson
    )

  implicit val methodNotFoundErrorDataEncoder: Codec[MethodNotFoundError.Data] = deriveCodec[MethodNotFoundError.Data]

  implicit val invalidParametersErrorErrorEncoder: Codec[InvalidParametersError.Error] =
    deriveCodec[InvalidParametersError.Error]

  implicit val invalidParametersErrorDataEncoder: Codec[InvalidParametersError.Data] =
    deriveCodec[InvalidParametersError.Data]

  implicit def internalJsonRpcErrorDataEncoder(implicit
    throwableEncoder: Encoder[ThrowableData]
  ): Encoder[InternalJsonRpcError.Data] =
    Encoder.forProduct2("reason", "throwable")(d => (d.reason, d.throwable))

  implicit def internalJsonRpcErrorEncoder(implicit
    throwableEncoder: Encoder[ThrowableData]
  ): Encoder[InternalJsonRpcError] =
    e => e.throwable.fold(encodeNoData(e))(encodeWithData(e, _))

  import ThrowableSupport._

  implicit val internalJsonRpcErrorDecoder: Decoder[InternalJsonRpcError] =
    Decoder.forProduct2("reason", "throwable")(InternalJsonRpcError.apply)

  implicit val invalidRequestErrorCodec: Codec[InvalidRequestError] =
    Codec.from(
      _.downField("data").downField("decodingFailure").as[DecodingFailure].map(InvalidRequestError.apply),
      e => encodeWithData(e, e.decodingFailure)
    )

  implicit val methodNotFoundErrorCodec: Codec[MethodNotFoundError] =
    Codec.from(
      _.downField("data").downField("method").as[String].map(MethodNotFoundError.apply),
      e => encodeWithData(e, MethodNotFoundError.Data(e.method))
    )

  implicit val invalidParametersErrorCodec: Codec[InvalidParametersError] =
    Codec.from(
      _.downField("data")
        .downField("parameterErrors")
        .as[NonEmptyChain[InvalidParametersError.Error]]
        .map(InvalidParametersError.apply),
      e => encodeWithData(e, InvalidParametersError.Data(e.parameterErrors))
    )

  implicit val customErrorCodec: Codec[CustomError] =
    deriveCodec[CustomError]

  implicit val rpcErrorDecoder: Decoder[RpcError] =
    c =>
      c.downField("code")
        .as[Int]
        .flatMap {
          case ParseError.code             => Right(ParseError)
          case InvalidRequestError.Code    => invalidRequestErrorCodec(c)
          case MethodNotFoundError.Code    => methodNotFoundErrorCodec(c)
          case InvalidParametersError.Code => invalidParametersErrorCodec(c)
          case InternalJsonRpcError.Code   => internalJsonRpcErrorDecoder(c)
          case _                           => customErrorCodec(c)
        }

  def encodeRpcData(a: RpcError)(implicit throwableEncoder: Encoder[ThrowableData]): Json =
    a match {
      case ParseError                => Json.Null
      case e: InvalidRequestError    => e.decodingFailure.asJson
      case e: MethodNotFoundError    => MethodNotFoundError.Data(e.method).asJson
      case e: InvalidParametersError => InvalidParametersError.Data(e.parameterErrors).asJson
      case e: InternalJsonRpcError   => e.throwable.fold(encodeNoData(e))(encodeWithData(e, _))
      case e: CustomError            => e.data
    }
}

object RpcErrorCodecs extends RpcErrorCodecs

object ThrowableSupport {

  implicit val decodeThrowableData: Decoder[ThrowableData] =
    Decoder.forProduct2("message", "stackTrace")(ThrowableData.apply)

  def verbose(verbose: Boolean): Codec[ThrowableData] =
    if (verbose) Verbose.verboseThrowableCodec else Standard.throwableCodec

  object Standard {

    implicit val throwableCodec: Codec[ThrowableData] =
      Codec.from(
        decodeThrowableData,
        throwable =>
          Map(
            "message" -> throwable.message.asJson
          ).asJson
      )
  }

  object Verbose {

    implicit val verboseThrowableCodec: Codec[ThrowableData] =
      Codec.from(
        decodeThrowableData,
        throwable =>
          Map(
            "message"    -> throwable.message.asJson,
            "stackTrace" -> throwable.stackTrace.asJson
          ).asJson
      )
  }
}
