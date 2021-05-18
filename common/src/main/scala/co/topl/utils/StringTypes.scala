package co.topl.utils

import cats.data.{NonEmptyChain, Validated, ValidatedNec}
import cats.implicits._
import cats.{Eq, Show}
import co.topl.utils.Extensions.StringOps
import co.topl.utils.encode.{Base16, Base58}
import io.circe.syntax.EncoderOps
import io.circe.{Decoder, Encoder, KeyDecoder, KeyEncoder}
import io.estatico.newtype.macros.newtype
import io.estatico.newtype.ops._

import scala.language.implicitConversions

object StringTypes {

  type StringValidationResult[A] = ValidatedNec[StringValidationFailure, A]

  @newtype
  class UTF8String(val value: String)

  object UTF8String {

    def validated(from: String): StringValidationResult[UTF8String] =
      Validated.condNec(from.getValidUTF8Bytes.isDefined, from.coerce, InvalidCharacterSet())

    def unsafe(from: String): UTF8String = from.coerce
  }

  trait UTF8StringInstances {
    implicit val eqUTF8String: Eq[UTF8String] = (a: UTF8String, b: UTF8String) => a.value === b.value

    implicit val showUTF8String: Show[UTF8String] = (value: UTF8String) => value.value

    implicit val jsonUTF8StringEncoder: Encoder[UTF8String] = (t: UTF8String) => t.value.asJson

    implicit val jsonUTF8StringDecoder: Decoder[UTF8String] =
      Decoder.decodeString.emap(UTF8String.validated(_).toEither.leftMap(_ => "Value is not a UTF-8 string"))

    implicit val keyEncodeUTF8String: KeyEncoder[UTF8String] = showUTF8String.show

    implicit val keyDecodeUTF8String: KeyDecoder[UTF8String] = UTF8String.validated(_).toOption
  }

  @newtype
  class Latin1String(val value: String)

  object Latin1String {

    def validated(from: String): StringValidationResult[Latin1String] =
      Validated.condNec(from.getValidLatin1Bytes.isDefined, from.coerce, InvalidCharacterSet())

    def unsafe(from: String): Latin1String = from.coerce
  }

  trait Latin1StringInstances {
    implicit val eqLatin1String: Eq[Latin1String] = (a: Latin1String, b: Latin1String) => a.value === b.value

    implicit val showLatin1String: Show[Latin1String] = (value: Latin1String) => value.value

    implicit val encodeLatin1String: Encoder[Latin1String] = (t: Latin1String) => t.value.asJson

    implicit val decodeLatin1String: Decoder[Latin1String] =
      Decoder.decodeString.emap(Latin1String.validated(_).toEither.leftMap(_ => "Value is not a Latin-1 string"))

    implicit val keyEncodeLatin1String: KeyEncoder[Latin1String] = showLatin1String.show

    implicit val keyDecodeLatin1String: KeyDecoder[Latin1String] = Latin1String.validated(_).toOption
  }

  @newtype
  class Base58String(val value: UTF8String)

  object Base58String {

    def validated(from: String): StringValidationResult[Base58String] =
      (for {
        validUtf8 <- UTF8String.validated(from).toEither
        isValidBase58 = Base58.isValidBase58(validUtf8)
        validBase58 <- Either.cond(isValidBase58, validUtf8.coerce, NonEmptyChain(InvalidCharacterSet()))
      } yield validBase58).toValidated

    def unsafe(from: String): Base58String = UTF8String.unsafe(from).coerce
  }

  trait Base58StringInstances {

    implicit val eqBase58String: Eq[Base58String] = (a: Base58String, b: Base58String) =>
      a.value.value === b.value.value

    implicit val showBase58String: Show[Base58String] = (value: Base58String) => value.value.value

    implicit val encodeBase58String: Encoder[Base58String] = (t: Base58String) => t.value.value.asJson

    implicit val decodeBase58String: Decoder[Base58String] =
      Decoder.decodeString
        .emap(Base58String.validated(_).toEither.leftMap(_ => "Value is not Base 58"))

    implicit val keyEncodeBase58String: KeyEncoder[Base58String] = showBase58String.show

    implicit val keyDecodeBase58String: KeyDecoder[Base58String] = Base58String.validated(_).toOption
  }

  @newtype
  class Base16String(val value: UTF8String)

  object Base16String {

    def validated(from: String): StringValidationResult[Base16String] =
      (for {
        validUtf8 <- UTF8String.validated(from).toEither
        isValidBase16 = Base16.isValid(validUtf8)
        validBase16 <- Either.cond(isValidBase16, validUtf8.coerce, NonEmptyChain(InvalidCharacterSet()))
      } yield validBase16).toValidated

    def validated(from: Array[Char]): StringValidationResult[Base16String] = validated(new String(from))

    def unsafe(from: String): Base16String = UTF8String.unsafe(from).coerce
  }

  trait Base16StringInstances {

    implicit val eqBase16String: Eq[Base16String] = (a: Base16String, b: Base16String) =>
      a.value.value === b.value.value

    implicit val showBase16String: Show[Base16String] = (value: Base16String) => value.value.value

    implicit val encodeBase16String: Encoder[Base16String] = (t: Base16String) => t.value.value.asJson

    implicit val decodeBase16String: Decoder[Base16String] =
      Decoder.decodeString.emap(Base16String.validated(_).toEither.leftMap(_ => "Value is not a Base 16 string"))

    implicit val keyEncodeBase16String: KeyEncoder[Base16String] = showBase16String.show

    implicit val keyDecodeBase16String: KeyDecoder[Base16String] = Base16String.validated(_).toOption
  }

  sealed trait StringValidationFailure
  final case class InvalidCharacterSet() extends StringValidationFailure

  object implicits
      extends UTF8StringInstances
      with Latin1StringInstances
      with Base58StringInstances
      with Base16StringInstances

}
