package co.topl.akkahttprpc

import cats.data.NonEmptyChain
import cats.implicits._
import io.circe._
import io.circe.generic.semiauto._
import io.circe.syntax._

trait RpcErrorCodecs {

  val encodeNoData: Encoder[RpcError[_]] =
    a => Map("code" -> a.code.asJson, "message" -> a.message.asJson).asJson

  implicit def encodeWithData[Data: Encoder, T <: RpcError[Data]]: Encoder[T] =
    a =>
      (Map("code" -> a.code.asJson, "message" -> a.message.asJson) ++ a.data.map(
        "data"    -> implicitly[Encoder[Data]].apply(_)
      )).asJson

  implicit val parseErrorCodec: Codec[ParseError.type] =
    Codec.from(
      _ => ParseError.asRight,
      encodeNoData
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

  implicit val decodeThrowableData: Decoder[ThrowableData] =
    Decoder.forProduct2("message", "stackTrace")(ThrowableData.apply)

  object ThrowableSupport {

    def verbose(verbose: Boolean): Codec[ThrowableData] =
      if(verbose) Verbose.verboseThrowableCodec else Standard.throwableCodec

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
    encodeWithData[InternalJsonRpcError.Data, InternalJsonRpcError]

  implicit val internalJsonRpcErrorDecoder: Decoder[InternalJsonRpcError] =
    Decoder.forProduct2("reason", "throwable")(InternalJsonRpcError.apply)

  implicit val invalidRequestErrorCodec: Codec[InvalidRequestError] =
    Codec.from(
      _.downField("data").downField("decodingFailure").as[DecodingFailure].map(InvalidRequestError.apply),
      encodeWithData[DecodingFailure, InvalidRequestError]
    )

  implicit val methodNotFoundErrorCodec: Codec[MethodNotFoundError] =
    Codec.from(
      _.downField("data").downField("method").as[String].map(MethodNotFoundError.apply),
      encodeWithData[MethodNotFoundError.Data, MethodNotFoundError]
    )

  implicit val invalidParametersErrorCodec: Codec[InvalidParametersError] =
    Codec.from(
      _.downField("data")
        .downField("parameterErrors")
        .as[NonEmptyChain[InvalidParametersError.Error]]
        .map(InvalidParametersError.apply),
      encodeWithData[InvalidParametersError.Data, InvalidParametersError]
    )

  implicit val customErrorCodec: Codec[CustomError] =
    deriveCodec[CustomError]

  implicit val rpcErrorDecoder: Decoder[RpcError[_]] =
    c =>
      c.downField("code")
        .as[Int]
        .flatMap {
          case ParseError.code             => Right(ParseError)
          case InvalidRequestError.Code    => invalidParametersErrorCodec(c)
          case MethodNotFoundError.Code    => methodNotFoundErrorCodec(c)
          case InvalidParametersError.Code => invalidParametersErrorCodec(c)
          case InternalJsonRpcError.Code   => internalJsonRpcErrorDecoder(c)
          case _                           => customErrorCodec(c)
        }

  implicit def rpcErrorEncoder(implicit throwableEncoder: Encoder[ThrowableData]): Encoder[RpcError[_]] =
    Encoder.instance {
      case ParseError                => parseErrorCodec.apply(ParseError)
      case i: InvalidRequestError    => invalidRequestErrorCodec(i)
      case i: MethodNotFoundError    => methodNotFoundErrorCodec(i)
      case i: InvalidParametersError => invalidParametersErrorCodec(i)
      case i: InternalJsonRpcError   => internalJsonRpcErrorEncoder.apply(i)
      case i: CustomError            => customErrorCodec(i)
    }
}

object RpcErrorCodecs extends RpcErrorCodecs
