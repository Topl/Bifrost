package co.topl.utils.codecs

import cats.data.ValidatedNec
import cats.implicits._
import co.topl.utils.StringDataTypes._
import co.topl.utils.StringDataTypes.implicits._
import co.topl.utils.encode.{Base16, Base58}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

import java.nio.charset.StandardCharsets

object StringDataTypesCodec {

  trait AsBytesInstances {
    implicit val base16BytesEncoder: AsBytes[Infallible, Base16Data] = AsBytes.infallible(_.value)

    implicit val base58BytesEncoder: AsBytes[Infallible, Base58Data] = AsBytes.infallible(_.value)

    implicit val latin1BytesEncoder: AsBytes[Infallible, Latin1Data] = AsBytes.infallible(_.value)
  }

  trait FromBytesInstances {
    implicit val base16BytesDecoder: FromBytes[Infallible, Base16Data] = FromBytes.infallible(Base16Data.fromData)

    implicit val base58BytesDecoder: FromBytes[Infallible, Base58Data] = FromBytes.infallible(Base58Data.fromData)

    implicit val latin1BytesDecoder: FromBytes[Infallible, Latin1Data] = FromBytes.infallible(Latin1Data.fromData)
  }

  trait JsonEncodingInstances {
    implicit val base16JsonEncoder: Encoder[Base16Data] = (t: Base16Data) => Base16.encode(t.value).asJson

    implicit val base58JsonEncoder: Encoder[Base58Data] = (t: Base58Data) => Base58.encode(t.value).asJson

    implicit val latin1JsonEncoder: Encoder[Latin1Data] = (t: Latin1Data) =>
      new String(t.value, StandardCharsets.ISO_8859_1).asJson
  }

  trait JsonDecodingInstances {

    implicit val base16JsonDecoder: Decoder[Base16Data] =
      Decoder[String].emap(Base16Data.validated(_).toEither.leftMap(_ => "Value is not a Base 16 string"))

    implicit val base58JsonDecoder: Decoder[Base58Data] =
      Decoder[String].emap(Base58Data.validated(_).toEither.leftMap(_ => "Value is not Base 58"))

    implicit val latin1JsonDecoder: Decoder[Latin1Data] =
      Decoder[String].emap(Latin1Data.validated(_).toEither.leftMap(_ => "Value is not a Latin-1 string"))
  }

  trait JsonKeyEncodingInstances {
    implicit val base16JsonKeyEncoder: KeyEncoder[Base16Data] = data => Base16.encode(data.value)

    implicit val base58JsonKeyEncoder: KeyEncoder[Base58Data] = data => Base58.encode(data.value)

    implicit val latin1JsonKeyEncoder: KeyEncoder[Latin1Data] =
      data => new String(data.value, StandardCharsets.ISO_8859_1)
  }

  trait JsonKeyDecodingInstances {
    implicit val base16JsonKeyDecoder: KeyDecoder[Base16Data] = Base16Data.validated(_).toOption

    implicit val base58JsonKeyDecoder: KeyDecoder[Base58Data] = Base58Data.validated(_).toOption

    implicit val latin1JsonKeyDecoder: KeyDecoder[Latin1Data] = Latin1Data.validated(_).toOption
  }

  trait Extensions {

    implicit class AsBytesInfallibleExtension[T](value: T)(implicit asBytes: AsBytes[Infallible, T]) {

      def encodeAsBase58: Base58Data =
        asBytes
          .encode(value)
          .map(Base58Data.fromData)
          .valueOr(e => throw new IllegalStateException(s"Infallible encoder failed: $e"))

      def encodeAsBase16: Base16Data =
        asBytes
          .encode(value)
          .map(Base16Data.fromData)
          .valueOr(e => throw new IllegalStateException(s"Infallible encoder failed: $e"))
    }

    implicit class AsBytesFallibleExtension[F, T](value: T)(implicit asBytes: AsBytes[F, T]) {

      def encodeAsValidatedBase58: ValidatedNec[F, Base58Data] = asBytes.encode(value).map(Base58Data.fromData)

      def encodeAsValidatedBase16: ValidatedNec[F, Base16Data] = asBytes.encode(value).map(Base16Data.fromData)
    }
  }

  trait StringTypesInstances
      extends AsBytesInstances
      with FromBytesInstances
      with JsonEncodingInstances
      with JsonDecodingInstances
      with JsonKeyEncodingInstances
      with JsonKeyDecodingInstances
      with Extensions

  object implicits extends StringTypesInstances
}
