package co.topl.utils

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

  type ValidationResult[A] = Either[StringValidationError, A]

  @newtype
  class UTF8String(val value: String)

  object UTF8String {

    def validated(from: String): ValidationResult[UTF8String] =
      Either.cond(from.getValidUTF8Bytes.isDefined, from.coerce, InvalidCharacterSet())

    def unsafe(from: String): UTF8String = from.coerce
  }

  trait UTF8StringImplicits {
    implicit val eqUTF8String: Eq[UTF8String] = (a: UTF8String, b: UTF8String) => a.value === b.value

    implicit val showUTF8String: Show[UTF8String] = (value: UTF8String) => value.value

    implicit val jsonUTF8StringEncoder: Encoder[UTF8String] = (t: UTF8String) => t.value.asJson

    implicit val jsonUTF8StringDecoder: Decoder[UTF8String] =
      Decoder.decodeString.emap(UTF8String.validated(_).leftMap(_ => "Value is not a UTF-8 string"))

    implicit val keyEncodeUTF8String: KeyEncoder[UTF8String] = showUTF8String.show

    implicit val keyDecodeUTF8String: KeyDecoder[UTF8String] = UTF8String.validated(_).toOption
  }

  @newtype
  class Latin1String(val value: String)

  object Latin1String {

    def validated(from: String): ValidationResult[Latin1String] =
      Either.cond(from.getValidLatin1Bytes.isDefined, from.coerce, InvalidCharacterSet())

    def unsafe(from: String): Latin1String = from.coerce
  }

  trait Latin1StringImplicits {
    implicit val eqLatin1String: Eq[Latin1String] = (a: Latin1String, b: Latin1String) => a.value === b.value

    implicit val showLatin1String: Show[Latin1String] = (value: Latin1String) => value.value

    implicit val encodeLatin1String: Encoder[Latin1String] = (t: Latin1String) => t.value.asJson

    implicit val decodeLatin1String: Decoder[Latin1String] =
      Decoder.decodeString.emap(Latin1String.validated(_).leftMap(_ => "Value is not a Latin-1 string"))

    implicit val keyEncodeLatin1String: KeyEncoder[Latin1String] = showLatin1String.show

    implicit val keyDecodeLatin1String: KeyDecoder[Latin1String] = Latin1String.validated(_).toOption
  }

  @newtype
  class Base58String(val value: UTF8String)

  object Base58String {

    def validated(from: String): ValidationResult[Base58String] =
      for {
        validUtf8 <- UTF8String.validated(from)
        isValidBase58 = Base58.isValidBase58(validUtf8)
        validBase58 <- Either.cond(isValidBase58, validUtf8.coerce, InvalidCharacterSet())
      } yield validBase58

    def unsafe(from: String): Base58String = UTF8String.unsafe(from).coerce
  }

  trait Base58StringImplicits {

    implicit val eqBase58String: Eq[Base58String] = (a: Base58String, b: Base58String) =>
      a.value.value === b.value.value

    implicit val showBase58String: Show[Base58String] = (value: Base58String) => value.value.value

    implicit val encodeBase58String: Encoder[Base58String] = (t: Base58String) => t.value.value.asJson

    implicit val decodeBase58String: Decoder[Base58String] =
      Decoder.decodeString
        .emap(Base58String.validated(_).leftMap(_ => "Value is not Base 58"))

    implicit val keyEncodeBase58String: KeyEncoder[Base58String] = showBase58String.show

    implicit val keyDecodeBase58String: KeyDecoder[Base58String] = Base58String.validated(_).toOption
  }

  @newtype
  class Base16String(val value: UTF8String)

  object Base16String {

    def validated(from: String): ValidationResult[Base16String] =
      for {
        validUtf8 <- UTF8String.validated(from)
        isValidBase16 = Base16.isValidBase16(validUtf8)
        validBase16 <- Either.cond(isValidBase16, validUtf8.coerce, InvalidCharacterSet())
      } yield validBase16

    def validated(from: Array[Char]): ValidationResult[Base16String] = validated(new String(from))

    def unsafe(from: String): Base16String = UTF8String.unsafe(from).coerce
  }

  trait Base16StringImplicits {

    implicit val eqBase16String: Eq[Base16String] = (a: Base16String, b: Base16String) =>
      a.value.value === b.value.value

    implicit val showBase16String: Show[Base16String] = (value: Base16String) => value.value.value

    implicit val encodeBase16String: Encoder[Base16String] = (t: Base16String) => t.value.value.asJson

    implicit val decodeBase16String: Decoder[Base16String] =
      Decoder.decodeString.emap(Base16String.validated(_).leftMap(_ => "Value is not a Base 16 string"))

    implicit val keyEncodeBase16String: KeyEncoder[Base16String] = showBase16String.show

    implicit val keyDecodeBase16String: KeyDecoder[Base16String] = Base16String.validated(_).toOption
  }

  sealed trait StringValidationError
  final case class InvalidCharacterSet() extends StringValidationError

  object implicits
      extends UTF8StringImplicits
      with Latin1StringImplicits
      with Base58StringImplicits
      with Base16StringImplicits

}
