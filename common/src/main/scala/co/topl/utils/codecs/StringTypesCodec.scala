package co.topl.utils.codecs

import cats.implicits._
import co.topl.utils.Extensions.StringOps
import co.topl.utils.StringTypes.implicits._
import co.topl.utils.StringTypes._
import co.topl.utils.encode.{Base16, Base58}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}

object StringTypesCodec {

  trait AsBytesInstances {
    implicit val base16BytesEncoder: AsBytes[Infallible, Base16String] = AsBytes.infallible(Base16.decode)

    implicit val base58BytesEncoder: AsBytes[Infallible, Base58String] = AsBytes.infallible(Base58.decode)

    implicit val utf8StringBytesEncoder: AsBytes[Infallible, UTF8String] =
      AsBytes.infallible(_.value.getValidUTF8Bytes.get)

    implicit val latin1BytesEncoder: AsBytes[Infallible, Latin1String] =
      AsBytes.infallible(_.value.getValidLatin1Bytes.get)
  }

  trait FromBytesInstances {
    implicit val base16BytesDecoder: FromBytes[Infallible, Base16String] = FromBytes.infallible(Base16.encode)

    implicit val base58BytesDecoder: FromBytes[Infallible, Base58String] = FromBytes.infallible(Base58.encode)

    implicit val utf8StringBytesDecoder: FromBytes[StringValidationFailure, UTF8String] =
      (bytes: Array[Byte]) => UTF8String.validated(new String(bytes))

    implicit val latin1BytesDecoder: FromBytes[StringValidationFailure, Latin1String] =
      (bytes: Array[Byte]) => Latin1String.validated(new String(bytes))
  }

  trait JsonEncodingInstances {
    implicit val base16JsonEncoder: Encoder[Base16String] = (t: Base16String) => t.value.value.asJson

    implicit val base58JsonEncoder: Encoder[Base58String] = (t: Base58String) => t.value.value.asJson

    implicit val utf8JsonEncoder: Encoder[UTF8String] = (t: UTF8String) => t.value.asJson

    implicit val latin1JsonEncoder: Encoder[Latin1String] = (t: Latin1String) => t.value.asJson
  }

  trait JsonDecodingInstances {

    implicit val base16JsonDecoder: Decoder[Base16String] =
      Decoder.decodeString.emap(Base16String.validated(_).toEither.leftMap(_ => "Value is not a Base 16 string"))

    implicit val base58JsonDecoder: Decoder[Base58String] =
      Decoder.decodeString.emap(Base58String.validated(_).toEither.leftMap(_ => "Value is not Base 58"))

    implicit val utf8JsonDecoder: Decoder[UTF8String] =
      Decoder.decodeString.emap(UTF8String.validated(_).toEither.leftMap(_ => "Value is not a UTF-8 string"))

    implicit val latin1JsonDecoder: Decoder[Latin1String] =
      Decoder.decodeString.emap(Latin1String.validated(_).toEither.leftMap(_ => "Value is not a Latin-1 string"))
  }

  trait JsonKeyEncodingInstances {
    implicit val base16JsonKeyEncoder: KeyEncoder[Base16String] = _.show

    implicit val base58JsonKeyEncoder: KeyEncoder[Base58String] = showBase58String.show

    implicit val utf8JsonKeyEncoder: KeyEncoder[UTF8String] = showUTF8String.show

    implicit val latin1JsonKeyEncoder: KeyEncoder[Latin1String] = showLatin1String.show
  }

  trait JsonKeyDecodingInstances {
    implicit val base16JsonKeyDecoder: KeyDecoder[Base16String] = Base16String.validated(_).toOption

    implicit val base58JsonKeyDecoder: KeyDecoder[Base58String] = Base58String.validated(_).toOption

    implicit val utf8JsonKeyDecoder: KeyDecoder[UTF8String] = UTF8String.validated(_).toOption

    implicit val latin1JsonKeyDecoder: KeyDecoder[Latin1String] = Latin1String.validated(_).toOption
  }

  trait StringTypesInstances
      extends AsBytesInstances
      with FromBytesInstances
      with JsonEncodingInstances
      with JsonDecodingInstances
      with JsonKeyEncodingInstances
      with JsonKeyDecodingInstances

  object implicits extends StringTypesInstances
}
